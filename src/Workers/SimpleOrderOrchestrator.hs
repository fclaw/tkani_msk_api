{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Workers.SimpleOrderOrchestrator (runSimpleOrderOrchestrator, notifyOrderChannelAboutError, sendErrorMessageToUser, try') where


import Katip (logTM, Severity(..), ls)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Maybe (fromJust, isJust)
import Data.Aeson ((.=), object, toJSON, Value (Null), eitherDecode)
import qualified Data.Text as T
import Data.Bifunctor (first)
import Control.Monad.IO.Class (liftIO)
import Network.Wreq (postWith, defaults, manager, responseBody)
import Control.Lens ((&), (.~), (^.))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Katip (logTM, Severity(..))
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Monad.Trans.Except (runExceptT)
import Control.Concurrent.Async.Lifted (async)


import Text (tshow)
import TH.Location (currentModule)
import Concurrency (runJobWithCleanup)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Database (storeTelegramMessageDetails, TelegramMessageDetails (..))
import qualified Workers.SimpleOrderOrchestrator.Order as Order
import API.Types (OrderRequest, OrderConfirmationDetails (..), orChatId)
import Infrastructure.Services.Telegram (disableLinkPreviewOption, ParseMode(MarkdownV2), MessageIdResponse (..), sendOrEditTelegramMessage)
import App (AppM, readTVarIO, readTChanIO, _simpleOrdersChan, _bots, ChatKey (MAIN, ORDER), _configHttpManager, render, _appDBPool)



try' :: IO a -> IO (Either SomeException a)
try' = try
{-# INLINE try' #-}


runSimpleOrderOrchestrator :: AppM ()
runSimpleOrderOrchestrator = do
  $(logTM) InfoS "Simple Order Orchestrator started."
  stVar <- get
  st <- readTVarIO stVar
  let inChan = _simpleOrdersChan st
  forever $
    -- Block and wait for a new order to appear in the channel
    readTChanIO inChan >>= (void . async . runJobWithCleanup . orchestrateSingleOrder)

orchestrateSingleOrder :: OrderRequest -> AppM ()
orchestrateSingleOrder order = do
  eitherRes <- (fmap (first tshow) . runExceptT . Order.place) order
  case eitherRes of
    Left err -> do
      msg <- render ($currentModule <> ".Error") mempty
      sendErrorMessageToUser (orChatId order) msg
      notifyOrderChannelAboutError err
      $(logTM) ErrorS $ "Failed to place order: " <> ls (tshow err)
    Right confirmationDetails -> 
      buildAndSendPaymentDetailsMessage
      (orChatId order)
      confirmationDetails

buildAndSendPaymentDetailsMessage :: Int64 -> OrderConfirmationDetails -> AppM ()
buildAndSendPaymentDetailsMessage chatId OrderConfirmationDetails {..} = do
  bots <- fmap _bots ask
  let (bot,_) = (M.!) bots MAIN
  let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendMessage"
  let templateData = HM.fromList [("orderId", orderId), ("trackingNumber", trackingNumber)]
  message <- fmap escapeMarkdownV2 $ render $currentModule templateData
  let buttons = 
        object [
          "inline_keyboard" .=
          [[ object 
            [ "text" .= ("💳 Оплатить картой" :: Text)
            , "url"  .= paymentLink
            ]
           ],
           [if isJust linkToQr then
              object 
              [ "text" .= ("📱 Оплатить СПБ" :: Text)
              , "url"  .= fromJust linkToQr
              ]
            else Null
           ]
          ]
        ]
  let payload =
        object
        [ "chat_id"              .= chatId
        , "text"                 .= message
        , "parse_mode"           .= tshow MarkdownV2
        , "link_preview_options" .= 
            disableLinkPreviewOption
        , "reply_markup"         .= buttons
        ]
  httpManager <- fmap _configHttpManager ask
  eTelMsgId <- liftIO $ try' $ postWith (defaults & manager .~ Right httpManager) url payload
  case eTelMsgId of
    Left err -> do
      msg <- render ($currentModule <> ".Error") mempty
      sendErrorMessageToUser chatId msg
      let error = escapeMarkdownV2 $ "‼️ Error in calling buildAndSendPaymentDetailsMessage: " <> tshow err
      void $ sendOrEditTelegramMessage mempty error ORDER Nothing Nothing Nothing
      $(logTM) ErrorS $ "Failed to send payment details message to Telegram: " <> ls (tshow err)
    Right response -> 
      let eitherMessageId = eitherDecode @MessageIdResponse (response ^. responseBody)
      in case eitherMessageId of
        Left err -> do
          msg <- render ($currentModule <> ".Error") mempty
          sendErrorMessageToUser chatId msg
          let error = escapeMarkdownV2 $ "‼️ Error in calling buildAndSendPaymentDetailsMessage(parse message id): " <> tshow err
          void $ sendOrEditTelegramMessage mempty error ORDER Nothing Nothing Nothing
          $(logTM) ErrorS $ "Failed to parse Telegram response for payment details message: " <> ls (tshow err)
        Right MessageIdResponse {..} -> do
          pool <- fmap _appDBPool ask
          let details = 
               TelegramMessageDetails
               { tmdSingleOrderId = Just orderId
               , tmdShelfOrderId  = Nothing
               , tmdChatId        = chatId
               , tmdMessageId     = message_id
               }
          void $ storeTelegramMessageDetails details pool

notifyOrderChannelAboutError :: Text -> AppM ()
notifyOrderChannelAboutError error = do
  let error = escapeMarkdownV2 $ "‼️ Error in calling orchestrateSingleOrder: " <> error
  void $ sendOrEditTelegramMessage mempty error ORDER Nothing Nothing Nothing


sendErrorMessageToUser :: Int64 -> Text -> AppM ()
sendErrorMessageToUser chatId error = do
  bots <- fmap _bots ask
  let (bot,_) = (M.!) bots MAIN
  let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendMessage"
  let message = escapeMarkdownV2 error
  let payload = object 
        [ "chat_id"              .= chatId
        , "text"                 .= message
        , "parse_mode"           .= tshow MarkdownV2
        , "link_preview_options" .= disableLinkPreviewOption
        ]
  httpManager <- fmap _configHttpManager ask
  void $ liftIO $ try' $ postWith (defaults & manager .~ Right httpManager) url payload