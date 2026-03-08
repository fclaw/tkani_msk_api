{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Workers.ShelfOrderRegister (runShelfOrderRegister) where


import Katip (logTM, Severity(..), ls)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Foldable (for_)
import Data.Aeson ((.=), object, eitherDecode)
import Katip (logTM, Severity(..))
import Data.Either (isLeft)
import Control.Monad (forever, void, when)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Concurrent.Async.Lifted (async)
import Control.Monad.IO.Class (liftIO)
import Network.Wreq (postWith, defaults, manager, responseBody)
import Control.Lens ((&), (.~), (^.))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M


import Text (tshow)
import TH.Location (currentModule)
import API.WithField (WithField (..))
import Concurrency (runJobWithCleanup)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import API.Types (InitiateShelfShipment, ShelfShipmentDetails (..), Providers (SDEK))
import qualified Workers.ShelfOrderRegister.Sdek as Sdek
import Infrastructure.Database (storeTelegramMessageDetails, TelegramMessageDetails (..))
import Infrastructure.Services.Telegram (disableLinkPreviewOption, ParseMode(MarkdownV2), MessageIdResponse (..))
import Workers.SimpleOrderOrchestrator (notifyOrderChannelAboutError, sendErrorMessageToUser, try')
import App (AppM, readTVarIO, readTChanIO, _shelfOrdersChan, render, _bots, ChatKey (MAIN), _configHttpManager, _appDBPool)


runShelfOrderRegister :: AppM ()
runShelfOrderRegister = do
  $(logTM) InfoS "Shelf Order Register started."
  stVar <- get
  st <- readTVarIO stVar
  let inChan = _shelfOrdersChan st
  -- Block and wait for a new order to appear in the channel
  forever $ readTChanIO inChan >>= (void . async . runJobWithCleanup . uncurry runSingleRegister)

runSingleRegister :: Int64 -> WithField "chat_id" Int64 InitiateShelfShipment -> AppM ()
runSingleRegister userId (WithField chatId init) = do
  $(logTM) InfoS $ ls $ "Processing shelf order for user " <> show userId
  eRes <- Sdek.place userId init
  case eRes of
    Left err -> do
      $(logTM) ErrorS $ "Failed to place order: " <> ls (tshow err)
      msg <- render ($currentModule <> ".Error") mempty
      sendErrorMessageToUser chatId msg
      notifyOrderChannelAboutError err
    Right maybeDetails -> for_ maybeDetails $ \details@ShelfShipmentDetails {..} -> do
      $(logTM) InfoS $ ls $ "Successfully registered shelf order for user " <> show userId <> " with details: " <> show details
      bots <- fmap _bots ask
      let (bot,_) = (M.!) bots MAIN
      let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendMessage"
      let templateData = 
            HM.fromList 
            [ ("orderId", ssdOrderId)
            , ("trackingNumber", fromJust ssdTrackingNumber)
            , ("provider", tshow ssdDeliveryProvider)
            ]
      message <- fmap escapeMarkdownV2 $ render $currentModule templateData
      let trackUrl = 
            case ssdDeliveryProvider of
              SDEK -> "https://www.cdek.ru/ru/tracking?order_id=" <> fromJust ssdTrackingNumber
              _    -> undefined -- We currently only support SDEK, but this is where you'd add more providers in the future. 
      let button = 
           object [
            "inline_keyboard" .=
            [[ object 
             [ "text" .= ("Отследить на сайте " <> tshow ssdDeliveryProvider)
             , "url"  .= trackUrl
             ]
            ]]
           ]
      let payload =
            object
            [ "chat_id"              .= chatId
            , "text"                 .= message
            , "parse_mode"           .= tshow MarkdownV2
            , "link_preview_options" .= 
                disableLinkPreviewOption
            , "reply_markup"         .= button
            ]
      httpManager <- fmap _configHttpManager ask
      eTelMsgId <- liftIO $ try' $ postWith (defaults & manager .~ Right httpManager) url payload
      case eTelMsgId of
        Left err -> do
          $(logTM) ErrorS $ "telegram failed to deliver message " <> ls (show eTelMsgId)
          notifyOrderChannelAboutError $ tshow eTelMsgId
        Right response -> do 
          $(logTM) InfoS $ "Successfully sent shelf order registration message to user " <> ls (show userId)
          let eitherMessageId = eitherDecode @MessageIdResponse (response ^. responseBody)
          case eitherMessageId of
            Left err -> do
              $(logTM) ErrorS $ "Failed to decode Telegram response: " <> ls (tshow err)
              notifyOrderChannelAboutError $ tshow err
            Right (MessageIdResponse msgId) -> do
              $(logTM) InfoS $ "Successfully decoded Telegram message ID: " <> ls (show msgId)
              -- Here you would typically want to save the msgId along with the order details in your database for future reference (e.g. if you want to edit or delete the message later). 
              pool <- fmap _appDBPool ask
              let details = 
                   TelegramMessageDetails
                   { tmdSingleOrderId = Just ssdOrderId
                   , tmdShelfOrderId  = Nothing
                   , tmdChatId        = chatId
                   , tmdMessageId     = msgId
                   }
              void $ storeTelegramMessageDetails details pool
