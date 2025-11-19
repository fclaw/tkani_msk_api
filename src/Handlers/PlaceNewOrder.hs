{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications  #-}

module Handlers.PlaceNewOrder(handler) where

import Katip
import Control.Monad.IO.Class (liftIO)
import Data.Time (formatTime, defaultTimeLocale, LocalTime)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Data.Bifunctor (first)
import Data.Traversable (for)
import Control.Monad (join, when, void)
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


import API.Types (OrderRequest (..), OrderConfirmationDetails (..), ApiResponse, defOrderConfirmationDetails, formatStatus, OrderStatus (New), mkError)
import App (AppM, currentTime, render, Config (..), runAppM)
import Infrastructure.Utils.OrderId (generateOrderId)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, MessageIdResponse)
import TH.Location (currentModule)
import qualified Infrastructure.Services.Sdek as Sdek
import qualified Infrastructure.Services.Sdek.Types as Sdek
import Infrastructure.Database (getFinalOrderItemPrice)
import Infrastructure.Services.Tinkoff as Tinkoff


data PlaceOrderError
  = SdekRegistrationFailed Sdek.SdekError  -- SDEK immediately rejected the payload
  | SdekConfirmationTimeout                -- The poller took too long to get a final status
  | TinkoffPaymentLinkFailed Tinkoff.TinkoffError -- Failed to create a payment link
  | DatabaseFailed Text -- Could not save the final order
  | SdekPollerError Text
  -- | NotificationSendFailed   Telegram.TelegramError -- (Optional) if you consider this a critical failure
  deriving (Show)


wrap action error = withExceptT error (ExceptT action)

sec :: Int
sec = 1000000

placeOrder :: OrderRequest -> ExceptT PlaceOrderError AppM OrderConfirmationDetails
placeOrder orderRequest@OrderRequest {..} = do

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
  orderId_ <- liftIO generateOrderId
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
  paymentLink <- wrap Tinkoff.generatePaymentLink TinkoffPaymentLinkFailed

  -- STEP C. Save the order in database

  -- STEP D. Notify the telegram channel

  return defOrderConfirmationDetails { orderId = orderId_}

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
  -- 1 register the order within sdek
    -- 1. Run the core business logic.
  eResult <- runExceptT (placeOrder newOrderRequest)
  -- 2. Pattern match on the result to build the final API response.
  case eResult of
    -- THE SUCCESS CASE
    Right newOrder -> do
      $(logTM) InfoS $ "Order placed successfully: " <> ls (orderId newOrder)
      -- After the DB commit, send the internal notification. This is a side effect.
      notifyOrdersChannel newOrderRequest (orderId newOrder)
      -- Return the successful response payload for the bot
      return $ Right newOrder
    -- THE FAILURE CASES
    Left err -> do
      -- Log the specific internal error
      $(logTM) ErrorS $ "Failed to place order: " <> ls (show err)
      -- Return a user-friendly, generic failure response
      return $ Left $ mkError "Failed to place order. See server logs for details."
      -- You can get more specific here if you want. For example:
      -- Left (SdekRegistrationFailed _) -> pure $ ApiFailure "Delivery provider error."
      -- Left (TinkoffPaymentLinkFailed _) -> pure $ ApiFailure "Payment system error."   


notifyOrdersChannel :: OrderRequest -> Text -> AppM ()
notifyOrdersChannel order orderId = do
  tm <- currentTime
  tz <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime tz tm
  -- Automatically finds and renders 'templates/Handlers/PlaceNewOrder.tpl'
  messageText <- render $currentModule $ buildTemplateData order localTime orderId
  eResult <- fmap (first (T.pack . show)) $ sendOrEditTelegramMessage ("new order: " <> orderId) messageText Nothing
  when(isLeft eResult) $ $(logTM) ErrorS $ "failed to deliver an order to telegram"      

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
      , ("status", formatStatus New)
      ]