{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TupleSections  #-}

module Handlers.PlaceNewOrder(handler) where

import Katip
import Control.Monad.IO.Class (liftIO)
import Data.Time (formatTime, defaultTimeLocale, LocalTime)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Text (Text)
import Data.Maybe
import Data.Bifunctor (first)
import Data.Traversable (for)
import Data.Foldable (for_)
import Control.Monad (join, when, void, msum)
import Control.Applicative (asum)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Except
import Data.Either (isLeft)
import qualified Data.UUID as UUID
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Class (get)
import Control.Monad.Reader.Class (ask)
import System.Timeout (timeout)
import Control.Concurrent (threadDelay)
import Data.List (find)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Concurrent.STM (writeTChan, atomically, readTVar)
import Data.Aeson.Encode.Pretty (encodePretty)



import API.Types (OrderRequest (..), OrderConfirmationDetails (..), ApiResponse, formatStatus, OrderStatus (Registered), mkError)
import App (AppM, currentTime, render, Config (..), runAppM, _tinkoffPaymentChan, ChatKey(ORDER), TinkoffCredentials (..), _tinkoffCred)
import Infrastructure.Utils.OrderId (generateOrderId)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, deleteMessage, MessageIdResponse (..))
import TH.Location (currentModule)
import qualified Infrastructure.Services.Sdek as Sdek
import qualified Infrastructure.Services.Sdek.Types as Sdek
import Infrastructure.Database (getFinalOrderItemPrice, placeNewOrder, insertNewPaymentRecord, NewPaymentRecord (..))
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Services.Tinkoff as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Security as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Init as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.GetState as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.QR as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Enum as Tinkoff
import Infrastructure.Services.Types (PaymentProvider (Tinkoff))
import Infrastructure.Utils.Http (HttpError)
import Text (encodeToText)


data PlaceOrderError
  = SdekRegistrationFailed Sdek.SdekError  -- SDEK immediately rejected the payload
  | SdekConfirmationTimeout                -- The poller took too long to get a final status
  | TinkoffHttpError HttpError     -- Failed to create a payment link
  | TinkoffPaymentLinkFailed Text     -- Failed to create a payment link with a textual error
  | TinkoffQrCodeFailed Text         -- Failed to generate QR code
  | DatabaseFailed Text -- Could not save the final order
  | SdekPollerError Text
  | NotificationSendFailed T.Text  -- (Optional) if you consider this a critical failure
  deriving (Show)


wrap action error = withExceptT error (ExceptT action)

sec :: Int
sec = 1000000

placeOrder :: OrderRequest -> MVar MessageIdResponse -> ExceptT PlaceOrderError AppM OrderConfirmationDetails
placeOrder orderRequest@OrderRequest {..} telegramIdVar = do

  cfg <- lift ask
  st <- lift get
  let pool = _appDBPool cfg
  let tariffCode = _sdekTariffCode cfg
  let shipmentPoint = _sdekShipmentPoint cfg
  -- fetch total price for a given fabric
  fabricPrice <- wrap (liftIO (getFinalOrderItemPrice orFabricId orPreCutId orLengthM pool)) DatabaseFailed
   -- STEP A. Register with SDEK (assuming this function returns Either SdekError ...)
  let minOderReq = Sdek.makeMinimalOrderRequestData orderRequest fabricPrice tariffCode shipmentPoint
  trackingUuid <- wrap (Sdek.registerOrder (Sdek.buildMinimalOderRequest minOderReq)) SdekRegistrationFailed
  orderId <- liftIO generateOrderId
  lift $ $(logTM) InfoS $ "SDEK request accepted. Waiting for final confirmation for UUID: " <> ls (UUID.toText trackingUuid)

  -- This is the action for our background poller thread.
  let thirtySeconds = 30 * sec
  let pollerAction = pollForSingleOrder cfg st trackingUuid
  -- We add a 30-second timeout to prevent the request from hanging forever.
  let maybeToEither (Just v) = Right v
      maybeToEither Nothing = Left ()
  lift $ $(logTM) InfoS $ "poller tries calling sdek for the final confirmation"
  ePollerRes <- wrap (liftIO (fmap maybeToEither (timeout thirtySeconds pollerAction))) (const SdekConfirmationTimeout)
  trackingNumber <- except $ (first SdekPollerError) ePollerRes
  
  -- STEP B. Generate the payment link
  let tinkoffCred = _tinkoffCred cfg
  let initReq = mkInitRequest orderId fabricPrice orderRequest tinkoffCred

  $(logTM) InfoS $ ls $ "initReq: " <> encodePretty initReq
  tinkoffResp :: Tinkoff.InitResponse <- wrap (Tinkoff.initiateTinkoffPayment initReq) TinkoffHttpError

  $(logTM) InfoS $ "Tinkoff response received. " <> ls (show tinkoffResp)

  when (Tinkoff.irSuccess tinkoffResp == False) $ do
    let errMsg = "Tinkoff Init API call failed: " <> fromMaybe "Unknown error" (Tinkoff.irMessage tinkoffResp)
    void $ wrap (pure (Left ())) (const $ TinkoffPaymentLinkFailed errMsg)

  paymentLink <- wrap ( 
    case Tinkoff.irPaymentURL tinkoffResp of
      Just link  -> pure (Right link)
      Nothing -> pure (Left ())
    ) (const $ TinkoffPaymentLinkFailed "Tinkoff Init API did not return a payment URL.")
   
  let tinkoffPaymentId = fromJust (Tinkoff.irPaymentId tinkoffResp)

  -- STEP C. Generate QR code
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

  -- STEP D. Notify the telegram channel
  telegramMsgId <- wrap (notifyOrdersChannel orderRequest orderId) NotificationSendFailed
  liftIO $ telegramIdVar `putMVar` telegramMsgId

  -- STEP E. Save the order in database
  let dbOrder = mkDbOrder orderRequest trackingUuid orderId trackingNumber telegramMsgId
  void $ wrap (liftIO (placeNewOrder dbOrder pool)) $ DatabaseFailed

  let newPaymentRecord = NewPaymentRecord
        { nprOrderId = orderId
        , nprProvider = Tinkoff
        , nprProviderPaymentId = tinkoffPaymentId
        , nprAmountKopecks = round fabricPrice
        , nprPaymentUrl = paymentLink
        , nprError = Nothing
        , nprToken = Tinkoff.irToken initReq
        }
  void $ wrap (liftIO (insertNewPaymentRecord newPaymentRecord pool)) DatabaseFailed

  -- forward paymentId to the poller
  let getStateRequest = 
        Tinkoff.GetStateRequest
        { gsrqPaymentId = tinkoffPaymentId
        , gsrqToken = 
            Tinkoff.generateGetStateToken $
              Tinkoff.GetStateToken
              tinkoffPaymentId
              (tinkoffTerminalKey tinkoffCred)
              (tinkoffSecret tinkoffCred)
        , gsrqTerminalKey = tinkoffTerminalKey tinkoffCred
        , gsrqIP = Nothing
        }
  liftIO $ atomically $ readTVar st >>= ((`writeTChan` (orderId, getStateRequest)) . _tinkoffPaymentChan)

  return OrderConfirmationDetails {..}

pollForSingleOrder cfg st uuid = do
  eRes <- runAppM cfg st (Sdek.getOrderStatus uuid)
  case eRes of
    Left err -> pure $ Left (T.pack (show err))
    Right (Right resp) -> do
      case Sdek.sosrRequests resp of
        [] -> do
          let errMsg = Sdek.SdekError "UNEXPECTED_RESPONSE" "SDEK status response did not contain our request UUID."
          runAppM cfg st $ $(logTM) ErrorS $ logStr $ "Polling error for " <> UUID.toText uuid <> ": " <> Sdek.seMessage errMsg
          pure $ Left (T.pack (show [errMsg]))
        (reqStatus : _) ->
          case Sdek.spsState reqStatus of
            Sdek.Accepted -> do
              -- The order is still processing. Wait and recurse.
              runAppM cfg st $ $(logTM) DebugS $ logStr $ "Polling " <> UUID.toText uuid <> ": Status is still ACCEPTED. Retrying..."
              threadDelay (3 * 1000000) -- Wait 3 seconds
              pollForSingleOrder cfg st uuid
            Sdek.Invalid -> do
              -- FINAL STATE: SDEK rejected the order.
              let errors = Sdek.spsErrors reqStatus
              runAppM cfg st $ $(logTM) WarningS $ logStr $ "Polling " <> UUID.toText uuid <> " resulted in INVALID state. Errors: " <> T.pack (show errors)
              -- Return the error result, which stops the loop.
              pure $ Left (T.pack (show errors))
            Sdek.Successful -> do
              -- FINAL STATE: SDEK accepted the order!
              let trackingNumber = fromJust $ Sdek.sosrCdekNumber resp -- As you noted
              runAppM cfg st $ $(logTM) InfoS $ logStr $ "Polling " <> UUID.toText uuid <> " resulted in SUCCESSFUL state. Tracking number: " <> trackingNumber
              pure $ Right trackingNumber
            other -> do
              let errMsg = Sdek.SdekError "UNEXPECTED_STATE" ("SDEK returned an unexpected final status: " <> T.pack (show other))
              runAppM cfg st $ $(logTM) ErrorS $ logStr $ "Polling error for " <> UUID.toText uuid <> ": " <> Sdek.seMessage errMsg
              pure $ Left (T.pack (show [errMsg]))
    Right (Left err) -> pure $ Left (T.pack (show err))  

-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: OrderRequest -> AppM (ApiResponse OrderConfirmationDetails)
handler newOrderRequest@OrderRequest {..} = do
  -- 1. Log the incoming request
  $(logTM) DebugS "Request received for creating a new order"
  $(logTM) InfoS "Handling new order request..."
  telegramIdVar <- liftIO newEmptyMVar
  -- 1. Run the core business logic.
  eResult <- runExceptT (placeOrder newOrderRequest telegramIdVar)
  -- 2. Pattern match on the result to build the final API response.
  case eResult of
    -- THE SUCCESS CASE
    Right newOrder -> do
      $(logTM) InfoS $ "Order placed successfully: " <> ls (orderId newOrder)
      -- Return the successful response payload for the bot
      return $ Right newOrder
    -- THE FAILURE CASES
    Left err -> do
      -- Log the specific internal error
      $(logTM) ErrorS $ "Failed to place order: " <> ls (show err)
      -- Return a user-friendly, generic failure response
      mMessageId <- liftIO $ tryTakeMVar telegramIdVar
      for_ (fmap coerce mMessageId) (flip deleteMessage ORDER)
      return $ Left $ mkError "Failed to place order. See server logs for details."  


notifyOrdersChannel :: OrderRequest -> Text -> AppM (Either T.Text MessageIdResponse)
notifyOrdersChannel order orderId = do
  tm <- currentTime
  tz <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime tz tm
  -- Automatically finds and renders 'templates/Handlers/PlaceNewOrder.tpl'
  messageText <- render $currentModule $ buildTemplateData order localTime orderId
  fmap (first (T.pack . show)) $ sendOrEditTelegramMessage ("new order: " <> orderId) messageText ORDER Nothing Nothing Nothing

-- | Escapes characters within a URL that can conflict with MarkdownV2 link parsing.
--   Primarily, we only need to worry about the closing parenthesis.
urlEncodeMarkdown :: T.Text -> T.Text
urlEncodeMarkdown = T.replace ")" "\\)"


-- | Builds a key-value map of template data from an order request and other context.
--   This map is used by the templating engine to render the final notification message.
buildTemplateData
  :: OrderRequest   -- ^ The original request data from the API call
  -> LocalTime      -- ^ The localized timestamp for the order
  -> Text           -- ^ The newly generated unique order ID
  -> HashMap Text Text
buildTemplateData order localTime orderId =
  let
    -- Format timestamp safely
    timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" localTime

    -- Determine purchase type
    purchaseType = if isJust (orLengthM order) then "Отрез по длине" else "Готовый отрез"

    -- Get length. We can use 'show' because the result will be a simple string.
    -- The 'asum' here is a clever way to pick the first 'Just' value.
    lengthStr = T.pack $ show $ fromMaybe 0 (asum [orPreCutLengthM order, orLengthM order])

    -- Safely construct the Telegram link for the template
    telegramLink = urlEncodeMarkdown (orTelegramUrl order)
    
  in
    -- Use HM.fromList for a clean construction of the map
    HM.fromList
      [ ("orderId", orderId)
      , ("timestamp", timeStr)
      , ("customerName", orCustomerFullName order)
      , ("customerPhone", orCustomerPhone order)
      -- fabricId is missing from your screenshot's OrderRequest, assuming it's available
      -- , ("fabricId", T.pack $ show $ orFabricId order) 
      , ("purchaseType", purchaseType)
      , ("length", lengthStr)
      , ("telegramLink", telegramLink)
      , ("deliveryProvider", T.pack $ show $ orDeliveryProviderId order)
      , ("deliveryPoint", orDeliveryPointId order)
      -- You might pass the status in, or hardcode it in the calling function
      , ("status", formatStatus Registered)
      ]


mkDbOrder :: OrderRequest -> UUID.UUID -> Text -> Text -> MessageIdResponse -> DB.Order
mkDbOrder OrderRequest {..} trackingUuid orderId trackingNumber telegramMsgId =
  DB.Order 
  { DB._orderId = orderId
  , DB._orderFabricId = orFabricId
  , DB._orderLengthM = orLengthM
  , DB._orderPreCutId = orPreCutId
  , DB._orderCustomerFullName = orCustomerFullName
  , DB._orderCustomerPhone = orCustomerPhone
  , DB._orderDeliveryProviderId = encodeToText orDeliveryProviderId
  , DB._orderDeliveryPointId = orDeliveryPointId
  , DB._orderTelegramUrl = orTelegramUrl
  , DB._orderSdekRequestUuid = trackingUuid
  , DB._orderSdekTrackingNumber = trackingNumber
  , DB._orderInternalNotificationMessageId = fromIntegral @Int @Int64 (coerce telegramMsgId)
  }


-- Helper for kopecks
toKopecks :: Double -> Int64
toKopecks = round . (* 100)

-- Helper function to remove characters that are not letters, numbers, punctuation, or spaces.
-- This will strip out emojis and other symbols.
sanitizeForGateway :: Text -> Text
sanitizeForGateway = T.filter (\c -> C.isLetter c || C.isNumber c || C.isPunctuation c || C.isSpace c)

mkInitRequest :: Text -> Double -> OrderRequest -> TinkoffCredentials -> Tinkoff.InitRequest
mkInitRequest orderId fabricPrice orderRequest tinkoffCred =
  let
     -- 'fabricPrice' is the TOTAL price in rubles. Convert it to kopecks once.
    totalAmountKopecks = toKopecks fabricPrice :: Int64

    (receiptItems, description) = case (orLengthM orderRequest, orPreCutId orderRequest) of
      
      -- CASE A: Roll Purchase
      (Just lengthM, Nothing) ->
        let
          -- 1. Back-calculate the price per meter
          -- If total is 3750 and length is 2.5, price/m is 1500
          pricePerMeter = if lengthM > 0 then fabricPrice / lengthM else 0
          pricePerMeterKopecks = toKopecks pricePerMeter

          item = Tinkoff.ReceiptItem
                  { riName = sanitizeForGateway (orFabricName orderRequest)
                  , riPrice = pricePerMeterKopecks -- Price of ONE unit (1 meter)
                  , riQuantity = lengthM            -- How many units
                  , riAmount = totalAmountKopecks  -- The pre-calculated total
                  , riTax = Tinkoff.None
                  , riPaymentMethod = Tinkoff.FullPayment
                  , riPaymentObject = Tinkoff.Commodity
                  }

          desc = 
            "Ткань на отрез: " <> 
            sanitizeForGateway (orFabricName orderRequest) <> 
            " (" <> 
            T.pack (show lengthM) <> " м x " <> 
            T.pack (show pricePerMeter) <> " руб/м)"

        in ([item], desc)

      -- CASE B: Pre-Cut Purchase
      (Nothing, Just _) ->
        let
          -- For a pre-cut, the total price IS the unit price
          item = Tinkoff.ReceiptItem
                  { riName = sanitizeForGateway (orFabricName orderRequest)
                  , riPrice = totalAmountKopecks -- Price of the one "item"
                  , riQuantity = 1.0
                  , riAmount = totalAmountKopecks -- Total is the same
                  , riTax = Tinkoff.VAT20
                  , riPaymentMethod = Tinkoff.FullPayment
                  , riPaymentObject = Tinkoff.Commodity
                  }
          
          desc = 
            "Мерный лоскут: " <> 
            sanitizeForGateway (orFabricName orderRequest) <> 
            " (1 шт. x " <> 
            T.pack (show fabricPrice) <> " руб)"

        in ([item], desc)
      
      _ -> ([], "Оплата заказа " <> orderId) -- Fallback

    -- Prepare token generation data
    terminalKey = tinkoffTerminalKey tinkoffCred
    terminalSecret = tinkoffSecret tinkoffCred
    tokenData = Tinkoff.InitToken (T.pack (show totalAmountKopecks)) orderId (Just description) terminalKey terminalSecret
    signature = Tinkoff.generatedInitToken tokenData
  in
  -- 3. Construct the final request
  Tinkoff.InitRequest
    { Tinkoff.irOrderId = orderId
    , Tinkoff.irTerminalKey = terminalKey
    , Tinkoff.irAmount = totalAmountKopecks
    , Tinkoff.irDescription = Just description
    , Tinkoff.irToken = signature
    , Tinkoff.irData = Just $
        Tinkoff.CustomerData
        { Tinkoff.cdEmail = Nothing
        , Tinkoff.cdPhone = Just (orCustomerPhone orderRequest)
        }
    , Tinkoff.irReceipt = Just $
        Tinkoff.ReceiptData
        { rdEmail = Nothing
        , rdPhone = Just (orCustomerPhone orderRequest)
        , rdTaxation = Tinkoff.USNIncome -- Or your specific system
        , rdItems = receiptItems
        }
    }