{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TupleSections  #-}

module API.Handlers.PlaceNewOrder(handler, mkInitRequest) where

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
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Class (get)
import Control.Monad.Reader.Class (ask)
import System.Timeout (timeout)
import Data.List (find)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Concurrent.STM (writeTChan, atomically, readTVar)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, takeTMVar)



import API.Types (OrderRequest (..), OrderConfirmationDetails (..), ApiResponse, formatStatus, OrderStatus (Registered), mkError)
import App (AppM, SdekJob (..), PaymentFlow (ShipNow), currentTime, render, Config (..), runAppM, _tinkoffPaymentChan, ChatKey(ORDER), TinkoffCredentials (..), _tinkoffCred, _sdekConfig, _appSdekChan)
import Infrastructure.Utils.OrderId (generateOrderId)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, deleteMessage, MessageIdResponse (..))
import TH.Location (currentModule)
import qualified Infrastructure.Services.Sdek as Sdek
import qualified Infrastructure.Services.Sdek.Types as Sdek
import Infrastructure.Database (getOrderItems, placeNewOrder, insertNewPaymentRecord, clearCart, NewPaymentRecord (..))
import qualified Infrastructure.Database as DB
import qualified Infrastructure.Services.Tinkoff as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Security as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Init as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.GetState as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.QR as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Enum as Tinkoff
import Infrastructure.Services.Types (PaymentProvider (Tinkoff))
import Infrastructure.Utils.Http (HttpError)
import Text (encodeToText, tshow)
import Domain.Warehouse.Types (FabricType (..))
import Utils.Telegram.Markdown (escapeMarkdownV2)
import qualified Infrastructure.Services.Sdek.Types.Config as Sdek



data PlaceOrderError
  = SdekRegistrationFailed Sdek.SdekError  -- SDEK immediately rejected the payload
  | SdekConfirmationTimeout -- SDEK did not confirm the order within the timeout period
  | SdekTariffNotFound
  | TinkoffHttpError HttpError     -- Failed to create a payment link
  | TinkoffPaymentLinkFailed Text     -- Failed to create a payment link with a textual error
  | TinkoffQrCodeFailed Text         -- Failed to generate QR code
  | DatabaseFailed Text -- Could not save the final order
  | SdekPollerError Text
  | NotificationSendFailed T.Text  -- (Optional) if you consider this a critical failure
  | CartEmpty
  deriving (Show)


wrap action error = withExceptT error (ExceptT action)


placeOrder :: OrderRequest -> MVar MessageIdResponse -> ExceptT PlaceOrderError AppM OrderConfirmationDetails
placeOrder orderRequest@OrderRequest {..} telegramIdVar = do

  cfg <- lift ask
  st <- lift get
  let pool = _appDBPool cfg
  let sdekConfig = _sdekConfig cfg
  let tariffCodes = Sdek.tariffs sdekConfig
  let senderLocation = Sdek.senderLocation sdekConfig
  let fromLocation =
        Sdek.defSdekFromLocation
        { Sdek.sflAddress = Sdek.address senderLocation
        , Sdek.sflCode = Sdek.cityCode senderLocation
        , Sdek.sflPostCode = Just $ Sdek.postalCode senderLocation
        }
  let shipmentPoint = Sdek.dropOffPoint sdekConfig

  when(orTariff `notElem` tariffCodes) $ undefined SdekTariffNotFound

  let maybeFromLocation | orTariff == 136 = Just fromLocation
                        | otherwise = Nothing
  let maybeShipmentPoint | orTariff == 138 = Just shipmentPoint
                         | otherwise = Nothing                  

  -- fetch total price for a given fabric
  items <- wrap (getOrderItems orTelegramUserId pool) DatabaseFailed

  when (length items == 0) $ except $ Left CartEmpty

   -- STEP A. Register with SDEK (assuming this function returns Either SdekError ...)
  let minOderReq = Sdek.makeMinimalOrderRequestData orderRequest items orTariff maybeFromLocation maybeShipmentPoint
  trackingUuid <- wrap (Sdek.registerOrder (Sdek.buildMinimalOderRequest minOderReq)) SdekRegistrationFailed
  orderId <- liftIO generateOrderId
  lift $ $(logTM) InfoS $ "SDEK request accepted. Waiting for final confirmation for UUID: " <> ls (UUID.toText trackingUuid)

  -- This is the action for our background poller thread.
  lift $ $(logTM) InfoS $ "poller tries calling sdek for the final confirmation"
  ePollerRes <- fetchOrderPollerRes trackingUuid
  trackingNumber <- except $ (first SdekPollerError) ePollerRes
  
  -- STEP B. Generate the payment link
  let tinkoffCred = _tinkoffCred cfg
  let initReq = mkInitRequest orderId items orCustomerPhone tinkoffCred

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
  telegramMsgId <- wrap (notifyOrdersChannel orderRequest items orderId) NotificationSendFailed
  liftIO $ telegramIdVar `putMVar` telegramMsgId

  -- STEP E. Save the order in database
  let dbOrder = mkDbOrder orderRequest trackingUuid orderId trackingNumber telegramMsgId
  void $ wrap (placeNewOrder dbOrder pool) $ DatabaseFailed

  let totalPrice = sum [ DB.oiTotalPrice item | item <- items]
  let newPaymentRecord = 
        NewPaymentRecord
        { nprOrderId           = Just orderId
        , nprProvider          = Tinkoff
        , nprProviderPaymentId = tinkoffPaymentId
        , nprAmountKopecks     = round totalPrice
        , nprPaymentUrl        = paymentLink
        , nprError             = Nothing
        , nprToken             = Tinkoff.irToken initReq
        , nprPaymentFlow       = encodeToText ShipNow
        , nprShelfOrderId       = Nothing
        }
  void $ wrap (insertNewPaymentRecord newPaymentRecord pool) DatabaseFailed

  -- STEP F. forward paymentId to the poller
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
  liftIO $ atomically $ readTVar st >>= ((`writeTChan` (ShipNow, orderId, getStateRequest)) . _tinkoffPaymentChan)

  -- clear out the cart
  void $ wrap (clearCart orTelegramUserId pool) DatabaseFailed

  return OrderConfirmationDetails {..}

fetchOrderPollerRes :: UUID -> ExceptT PlaceOrderError AppM (Either Text Text)
fetchOrderPollerRes uuid = do
  st <- get
  inChan <- fmap _appSdekChan $ liftIO $ atomically $ readTVar st -- The poller's INput chan
  -- 1. Create a new, empty TMVar for the reply
  replyVar <- liftIO newEmptyTMVarIO

  -- 2. Create the job and put it on the poller's queue
  let job = SdekJob uuid replyVar
  liftIO $ atomically $ writeTChan inChan job

  -- 3. Block and wait for the result to appear in our reply box
  -- We use a timeout to prevent waiting forever.
  mResult <- liftIO $ timeout (30 * 1000000) $ atomically $ takeTMVar replyVar

  -- 4. Handle the outcome
  case mResult of
    -- Timeout occurred
    Nothing -> throwE SdekConfirmationTimeout
        
    -- We got a result from the poller
    Just result -> return result

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


notifyOrdersChannel :: OrderRequest -> [DB.OrderItem] -> Text -> AppM (Either T.Text MessageIdResponse)
notifyOrdersChannel order items orderId = do
  tm <- currentTime
  tz <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime tz tm
  -- Automatically finds and renders 'templates/Handlers/PlaceNewOrder.tpl'
  messageText <- render $currentModule $ buildTemplateData orderId localTime order items
  fmap (first (T.pack . show)) $ sendOrEditTelegramMessage ("new order: " <> orderId) (escapeMarkdownV2 messageText) ORDER Nothing Nothing Nothing


-- | Builds the key-value map for the multi-item admin notification template.
buildTemplateData
  :: Text             -- ^ The generated Order ID
  -> LocalTime        -- ^ The localized timestamp
  -> OrderRequest     -- ^ Contains customer & delivery info
  -> [DB.OrderItem]   -- ^ The list of items in the cart
  -> HashMap Text Text
buildTemplateData orderId localTime orderReq items =
  let
    -- 1. Format common values
    timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" localTime
    itemCount = T.pack $ show $ length items

    -- 2. Build the 'itemsBlock' by mapping over the list
    itemLines = map formatOrderItemLine items
    itemsBlock = T.unlines itemLines
    
  in
    -- 3. Construct the final HashMap
    HM.fromList
      [ ("orderId", orderId)
      , ("timestamp", timeStr)
      , ("customerName", orCustomerFullName orderReq)
      , ("customerPhone", orCustomerPhone orderReq)
      
      -- NEW: Variables for the item list
      , ("itemCount", itemCount)
      , ("itemsBlock", itemsBlock)

      -- Delivery & Status
      , ("deliveryProvider", T.pack $ show $ orDeliveryProviderId orderReq) -- Assuming this is already Text
      , ("deliveryPoint", orDeliveryPointId orderReq)
      , ("status", formatStatus Registered) -- You can format this from an Enum if you have one
      ]

-- | Helper function to format a single line in the 'itemsBlock'.
--   (This can be in the same module or imported)
formatOrderItemLine :: DB.OrderItem -> Text
formatOrderItemLine item =
    let
        name = DB.oiName item -- Assume it's already sanitized
        pricePerMetre = fromMaybe 0 $ DB.oiPricePerMetre item
        -- Create a detail string for rolls
        totalPrice = case DB.oiFabricType item of -- Assuming DB.OrderItem has an 'oiType'
            Roll ->
                let len = fromMaybe 0.0 (DB.oiLengthM item)
                in "Отрез " <> T.pack (show len) <> " м * " <> T.pack (show pricePerMetre) <> " руб/м"
            PreCut ->
                -- For pre-cuts, the name often already includes the length
                T.pack (show (DB.oiTotalPrice item)) <> " руб"
    in
    "• " <> name <> " | " <> totalPrice <> " | " <> DB.oiArticle item <> "\n"


mkDbOrder :: OrderRequest -> UUID.UUID -> Text -> Text -> MessageIdResponse -> DB.Order
mkDbOrder OrderRequest {..} trackingUuid orderId trackingNumber telegramMsgId =
  DB.Order 
  { DB._orderId = orderId
  , DB._orderCustomerFullName = orCustomerFullName
  , DB._orderCustomerPhone = orCustomerPhone
  , DB._orderDeliveryProviderId = encodeToText orDeliveryProviderId
  , DB._orderDeliveryPointId = orDeliveryPointId
  , DB._orderSdekRequestUuid = trackingUuid
  , DB._orderSdekTrackingNumber = trackingNumber
  , DB._orderInternalNotificationMessageId = coerce telegramMsgId
  , DB._orderTelegramUserId = orTelegramUserId
  , DB._orderTariff = fromIntegral orTariff
  }


-- Helper for kopecks
toKopecks :: Double -> Int64
toKopecks = round . (* 100)

-- Helper function to remove characters that are not letters, numbers, punctuation, or spaces.
-- This will strip out emojis and other symbols.
sanitizeForGateway :: Text -> Text
sanitizeForGateway = T.filter (\c -> C.isLetter c || C.isNumber c || C.isPunctuation c || C.isSpace c)


-- | Converts a single internal OrderItem into a Tinkoff ReceiptItem for fiscalization.
mkReceiptItem :: DB.OrderItem -> Tinkoff.ReceiptItem
mkReceiptItem DB.OrderItem{..} =
  -- Use the explicit type field to determine the logic.
  case oiFabricType of
    
    -- === It's a Roll cut ===
    Roll ->
      let
        -- For a roll, the 'price' is per meter.
        pricePerMeterKopecks = toKopecks (fromMaybe 0 oiPricePerMetre)
        totalAmountKopecks = toKopecks oiTotalPrice
        lengthM = fromMaybe 0 oiLengthM
      in
      Tinkoff.ReceiptItem
        { riName = sanitizeForGateway oiName
        , riPrice = pricePerMeterKopecks -- Price of ONE unit (1 meter)
        , riQuantity = lengthM            -- How many units were purchased
        , riAmount = totalAmountKopecks   -- The total for this line item
        , riTax = Tinkoff.None
        , riPaymentMethod = Tinkoff.FullPayment
        , riPaymentObject = Tinkoff.Commodity
        }
        
    -- === It's a Pre-Cut ===
    PreCut ->
      let
        -- For a pre-cut, the total price IS the unit price.
        totalPriceKopecks = toKopecks oiTotalPrice
      in
      Tinkoff.ReceiptItem
        { riName = sanitizeForGateway oiName
        , riPrice = totalPriceKopecks -- Unit price is the total price
        , riQuantity = 1.0              -- We are selling 1 "piece"
        , riAmount = totalPriceKopecks   -- Total is the same
        , riTax = Tinkoff.None
        , riPaymentMethod = Tinkoff.FullPayment
        , riPaymentObject = Tinkoff.Commodity
        }

-- | Formats a single OrderItem into a short, descriptive line for the Description field.
formatDescriptionLine :: (Int, DB.OrderItem) -> Text
formatDescriptionLine (index, item) =
    let
        -- Keep the name short for the description
        itemName = T.take 50 (DB.oiName item) -- Truncate long names
        details = case DB.oiFabricType item of
            Roll   -> 
              T.pack (show (fromMaybe 0.0 (DB.oiLengthM item))) <> 
              "м x " <> 
              T.pack (show (fromJust (DB.oiPricePerMetre item))) <> 
              " руб/м"
            PreCut -> "1 шт. x " <> T.pack (show (DB.oiTotalPrice item)) <> " руб"
    in
    (T.pack $ show index) <> ". " <> itemName <> " (" <> details <> ")\n"


-- | Main function to construct the Tinkoff InitRequest for a multi-item order.
mkInitRequest :: Text -> [DB.OrderItem] -> Text -> TinkoffCredentials -> Tinkoff.InitRequest
mkInitRequest orderId items customerPhone tinkoffCred =
  let
    -- 1. Create a fiscal receipt item for EACH item in the cart.
    receiptItems = map mkReceiptItem items

    -- 2. Calculate the total amount for the entire order.
    totalAmountKopecks = sum $ map Tinkoff.riAmount receiptItems

    -- 3. === BUILD THE DESCRIPTION LIST ===
    --    First, format each item into a line.
    --    We use 'zip [1..]' to get item numbers (1., 2., 3., etc.).
    itemLines = map formatDescriptionLine (zip [1..] items)
    --    Combine the lines into a single block.
    itemListText = T.unlines itemLines
    --    Create the final description, truncating if it's too long to be safe.
    --    Tinkoff's limit is usually ~250 chars. We'll use 240 as a safe buffer.
    description = T.take 240 ("Заказ " <> orderId <> ":\n" <> itemListText)

    -- 4. Prepare data for token generation.
    terminalKey = tinkoffTerminalKey tinkoffCred
    terminalSecret = tinkoffSecret tinkoffCred
    tokenData = Tinkoff.InitToken
                  (T.pack $ show totalAmountKopecks)
                  orderId
                  (Just description)
                  terminalKey
                  terminalSecret
    signature = Tinkoff.generatedInitToken tokenData
    customerData = Tinkoff.defCustomerData { Tinkoff.cdPhone = Just customerPhone }
    receiptData = Tinkoff.defReceiptData {Tinkoff.rdPhone = Just customerPhone, Tinkoff.rdItems = receiptItems}
  in
     -- 5. Construct the final request.
  Tinkoff.InitRequest
    { Tinkoff.irOrderId = orderId
    , Tinkoff.irTerminalKey = terminalKey
    , Tinkoff.irAmount = totalAmountKopecks
    , Tinkoff.irDescription = Just description
    , Tinkoff.irToken = signature
    , Tinkoff.irData = Just customerData
    , Tinkoff.irReceipt = Just receiptData
    }