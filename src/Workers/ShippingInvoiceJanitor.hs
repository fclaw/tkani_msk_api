{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workers.ShippingInvoiceJanitor (runShippingInvoiceJanitor) where

import Data.Text (Text)
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import Data.Bifunctor (first, bimap)
import Control.Monad.Trans.Except
import Data.Maybe (fromMaybe, fromJust)
import Katip (logTM, Severity(..), ls)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Char as C
import System.Timeout (timeout)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Concurrent.Async.Lifted (async)
import Control.Monad (forever, void, when)
import Network.Wreq (postWith, defaults, manager, responseBody)
import Control.Lens ((&), (.~), (^.))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Aeson ((.=), object, toJSON, Value (Null), eitherDecode)
import Control.Concurrent.STM (atomically, readTVar, writeTChan)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, takeTMVar)



import App
import Text (tshow)
import TH.Location (currentModule)
import Infrastructure.Utils.OrderId (generateOrderId)
import Concurrency (runJobWithCleanup)
import Infrastructure.Utils.Http (HttpError)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Database (fetchShipmentCost, insertShipmentPaymentRecord, ShipmentPaymentRecord (..))
import API.Types (YandexShipmentFinalizeReq (..))
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, disableLinkPreviewOption, ParseMode(MarkdownV2), MessageIdResponse (..))
import Workers.SimpleOrderOrchestrator (sendErrorMessageToUser, try')
import qualified Infrastructure.Services.Tinkoff as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Security as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Init as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.GetState as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.QR as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Enum as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Cancel as Tinkoff
import Infrastructure.Services.Types (PaymentProvider (Tinkoff))


runShippingInvoiceJanitor :: AppM ()
runShippingInvoiceJanitor = do
  $(logTM) InfoS "Shipping invoice janitor started."
  stVar <- get
  st <- readTVarIO stVar
  let inChan = _shipmentChan st
  -- Block and wait for a new order to appear in the channel
  forever $ readTChanIO inChan >>= (void . async . runJobWithCleanup . runSingleShippingInvoice)


runSingleShippingInvoice :: YandexShipmentFinalizeReq -> AppM ()
runSingleShippingInvoice req@YandexShipmentFinalizeReq {..} = do
  $(logTM) InfoS $ "YandexShipmentFinalizeReq --> " <> ls (tshow req)
  eInvoiceRes <- runExceptT $ action req
  case eInvoiceRes of
    Left err -> do
      $(logTM) ErrorS $
        "Failed to generate invoice details: " <> 
        ls (tshow err)
      msg <- render ($currentModule <> ".Error") mempty
      sendErrorMessageToUser ysfrChatId msg
      notifyPrepaidOrderChannelAboutError err
    Right _ -> $(logTM) InfoS $ "runSingleShippingInvoice has finished ..."

notifyPrepaidOrderChannelAboutError :: ShipmentInvoiceError -> AppM ()
notifyPrepaidOrderChannelAboutError error = do
  let errorMsg = escapeMarkdownV2 $ "‼️ Error in calling runSingleShippingInvoice: " <> (tshow error)
  void $ sendOrEditTelegramMessage mempty errorMsg PREPAID_ORDER Nothing Nothing Nothing

data ShipmentInvoiceError = 
       DatabaseFailed Text
     | ShipmentCostNotDetermined
     | TinkoffHttpError HttpError
     | TinkoffPaymentLinkFailed Text
     | TelegramSendFailed Text
     deriving (Show)

wrap action error = withExceptT error (ExceptT action)
{-# INLINE wrap #-}

wrapOrCancel :: AppM (Either e a) -> (e -> ShipmentInvoiceError) -> AppM () -> ExceptT ShipmentInvoiceError AppM a
wrapOrCancel action errorWrapper cleanup = wrap action errorWrapper `catchE` \err -> lift cleanup >> throwE err
{-# INLINE wrapOrCancel #-}

action :: YandexShipmentFinalizeReq -> ExceptT ShipmentInvoiceError AppM ()
action YandexShipmentFinalizeReq {..} = do
  cfg <- lift ask
  let pool = _appDBPool cfg
  mShipmentCost <- wrap (fetchShipmentCost ysfrOrderId pool) DatabaseFailed
  case mShipmentCost of
    Nothing -> throwE ShipmentCostNotDetermined
    Just (shipmentCost, customerPhone) -> do
      tmpOrderId <- liftIO generateOrderId
      let shipmentOrderId = tmpOrderId <> "-SHIP"
      -- STEP A. Generate the payment link
      let tinkoffCred = _tinkoffCred cfg
      let initReq = mkInitRequest shipmentOrderId shipmentCost customerPhone tinkoffCred
      $(logTM) InfoS $ ls $ "initReq: " <> encodePretty initReq
      tinkoffResp :: Tinkoff.InitResponse <- wrap (Tinkoff.initiateTinkoffPayment initReq) TinkoffHttpError
      $(logTM) InfoS $ "Tinkoff response received. " <> ls (show tinkoffResp)

      when (Tinkoff.irSuccess tinkoffResp == False) $ do
        let errMsg = 
              "Tinkoff Init API call failed: " <> 
              fromMaybe 
              "Unknown error" 
              (Tinkoff.irMessage tinkoffResp)
        void $ wrap (pure (Left ())) (const $ TinkoffPaymentLinkFailed errMsg)

      paymentLink <- 
        wrap ( 
          case Tinkoff.irPaymentURL tinkoffResp of
            Just link  -> pure (Right link)
            Nothing -> pure (Left ())
        ) $ \_ -> TinkoffPaymentLinkFailed "Tinkoff Init API did not return a payment URL."
   
      let tinkoffPaymentId = fromJust (Tinkoff.irPaymentId tinkoffResp)

      -- STEP B. Generate QR code
      let qrReq = 
           Tinkoff.defGetQrRequest 
           { Tinkoff.gqrTerminalKey = tinkoffTerminalKey tinkoffCred
           , Tinkoff.gqrPaymentId = read @Int64 (T.unpack tinkoffPaymentId)
           , Tinkoff.gqrToken =
             Tinkoff.generateGetQrToken $
               Tinkoff.GetQrToken
               tinkoffPaymentId
               (tinkoffTerminalKey tinkoffCred)
               (tinkoffSecret tinkoffCred)
               Tinkoff.PAYLOAD
           }

      $(logTM) InfoS $ ls $ "QR req: " <> encodePretty qrReq
      tinkoffQrResp :: Tinkoff.GetQrResponse <- wrap (Tinkoff.getTinkoffQRCode qrReq) TinkoffHttpError

      when(Tinkoff.gqrrSuccess tinkoffQrResp == False) $
        $(logTM) ErrorS $ "Tinkoff QR fails. " <> ls (show tinkoffQrResp)

      let linkToQr = Tinkoff.gqrrData tinkoffQrResp

      messageId <- wrap (buildAndSendPaymentDetailsMessage shipmentCost paymentLink linkToQr shipmentOrderId ysfrChatId) TelegramSendFailed
      
      let shipmentPaymentRecord = 
            ShipmentPaymentRecord
            { sprOrderId           = shipmentOrderId
            , sprParcelOrderId     = ysfrOrderId
            , sprProvider          = Tinkoff
            , sprProviderPaymentId = tinkoffPaymentId
            , sprAmountKopecks     = shipmentCost
            , sprPaymentUrl        = paymentLink
            , sprError             = Nothing
            , sprToken             = Tinkoff.irToken initReq
            , sprChatId            = ysfrChatId
            , sprMessageId         = messageId
            }

      let cancel = 
            Tinkoff.cancelTinkoffPayment $ 
             Tinkoff.CancelRequest 
              (tinkoffTerminalKey tinkoffCred)
              tinkoffPaymentId
              (Tinkoff.generateGetStateToken $
               Tinkoff.GetStateToken
               tinkoffPaymentId
               (tinkoffTerminalKey tinkoffCred)
               (tinkoffSecret tinkoffCred))
      void $ wrapOrCancel (insertShipmentPaymentRecord shipmentPaymentRecord pool) DatabaseFailed $ void $ cancel

      -- STEP C. forward paymentId to the poller
      let getStateRequest = 
           Tinkoff.GetStateRequest
           { gsrqPaymentId = tinkoffPaymentId
           , gsrqToken = 
              Tinkoff.generateGetStateToken $
               Tinkoff.GetStateToken
               tinkoffPaymentId
               (tinkoffTerminalKey tinkoffCred)
               (tinkoffSecret tinkoffCred)
           , gsrqTerminalKey = 
             tinkoffTerminalKey tinkoffCred
           , gsrqIP = Nothing
           }
      lift $ runTinkoffPaymentStatusPoller shipmentOrderId getStateRequest ysfrChatId messageId

-- Helper function to remove characters that are not letters, numbers, punctuation, or spaces.
-- This will strip out emojis and other symbols.
sanitizeForGateway :: Text -> Text
sanitizeForGateway = T.filter (\c -> C.isLetter c || C.isNumber c || C.isPunctuation c || C.isSpace c)


mkInitRequest :: Text -> Int32 -> Text -> TinkoffCredentials -> Tinkoff.InitRequest
mkInitRequest orderId totalAmountKopecks customerPhone tinkoffCred =
  let kopecksToInt64 = fromIntegral totalAmountKopecks
      terminalKey = tinkoffTerminalKey tinkoffCred
      terminalSecret = tinkoffSecret tinkoffCred
      description  = "Оплата доставки заказа №" <> orderId <> " (Tkani MSK)"
      tokenData = 
        Tinkoff.InitToken
        (tshow totalAmountKopecks)
        orderId
        (Just description)
        terminalKey
        terminalSecret
      signature = Tinkoff.generatedInitToken tokenData
      customerData = 
        Tinkoff.defCustomerData 
        { Tinkoff.cdPhone = 
          Just customerPhone 
        }
      receiptItems =
        Tinkoff.ReceiptItem
        { riName = sanitizeForGateway description
        , riPrice = kopecksToInt64 -- Unit price is the total price
        , riQuantity = 1.0              -- We are selling 1 "piece"
        , riAmount = kopecksToInt64   -- Total is the same
        , riTax = Tinkoff.None
        , riPaymentMethod = Tinkoff.FullPayment
        , riPaymentObject = Tinkoff.Service
        }
      receiptData =
        Tinkoff.defReceiptData 
        { Tinkoff.rdPhone = Just customerPhone, 
          Tinkoff.rdItems = [receiptItems]
        }
  in Tinkoff.InitRequest
     { Tinkoff.irOrderId = orderId
     , Tinkoff.irTerminalKey = terminalKey
     , Tinkoff.irAmount = kopecksToInt64
     , Tinkoff.irDescription = Just description
     , Tinkoff.irToken = signature
     , Tinkoff.irData = Just customerData
     , Tinkoff.irReceipt = Just receiptData
     }

buildAndSendPaymentDetailsMessage :: Int32 -> Text -> Maybe Text -> Text -> Int64 -> AppM (Either Text Int64)
buildAndSendPaymentDetailsMessage totalAmountKopecks paymentLink linkToQr orderId chatId = do
  bots <- fmap _bots ask
  let (bot,_) = (M.!) bots MAIN
  let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendMessage"
  let templateData = 
        HM.fromList 
        [ ("orderId", orderId)
        , ("price", tshow (fromRational (fromIntegral totalAmountKopecks) / 100.0))
        ]
  message <- fmap escapeMarkdownV2 $ render ($currentModule <> ".Payment") templateData
  let cardRow = [ object [ "text" .= ("💳 Оплатить картой" :: Text), "url" .= paymentLink ] ]  
  -- Create the SBP row ONLY if the link exists, otherwise return an empty list
  let sbpRow =
        case linkToQr of
          Just qr -> [[ object [ "text" .= ("📱 Оплатить СПБ" :: Text), "url" .= qr ] ]]
          Nothing -> []

  let buttons = object ["inline_keyboard" .= ([cardRow] ++ sbpRow)]
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
    Left err -> pure $ Left $ tshow err
    Right response -> do
      let eitherMessageId = 
            eitherDecode @MessageIdResponse 
            (response ^. responseBody)
      pure $ bimap tshow message_id $ eitherMessageId


runTinkoffPaymentStatusPoller :: Text -> Tinkoff.GetStateRequest -> Int64 -> Int64 -> AppM ()
runTinkoffPaymentStatusPoller orderId req chatId msgId = do
  st <- get
  inChan <- fmap _tinkoffShipmentChan $ readTVarIO st -- The poller's INput chan
  -- 2. Create the job and put it on the poller's queue
  let job = TinkoffShipmentPaymentJob orderId req chatId msgId
  writeTChanIO inChan job