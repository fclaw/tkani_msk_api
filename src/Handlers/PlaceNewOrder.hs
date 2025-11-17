{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards  #-}

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
import Control.Monad (join, when)
import Control.Applicative (asum)
import Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Except
import Data.Either (isLeft)


import API.Types (OrderRequest (..), OrderConfirmationDetails (..), ApiResponse, defOrderConfirmationDetails, formatStatus, OrderStatus (New), mkError)
import App (AppM, currentTime, render)
import Infrastructure.Utils.OrderId (generateOrderId)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, MessageIdResponse)
import TH.Location (currentModule)
import qualified Infrastructure.Services.Sdek as Sdek


data PlaceOrderError
  = SdekRegistrationFailed Sdek.SdekError  -- SDEK immediately rejected the payload
  -- | SdekConfirmationTimeout                -- The poller took too long to get a final status
  -- | SdekConfirmationInvalid [Sdek.SdekError] -- The poller got an INVALID final status
  -- | TinkoffPaymentLinkFailed Tinkoff.TinkoffError -- Failed to create a payment link
  -- | DatabaseInsertFailed     Pool.UsageError -- Could not save the final order
  -- | NotificationSendFailed   Telegram.TelegramError -- (Optional) if you consider this a critical failure
  deriving (Show)


wrap action error = withExceptT error (ExceptT action)

placeOrder :: OrderRequest -> ExceptT PlaceOrderError AppM OrderConfirmationDetails
placeOrder OrderRequest {..} = do 
   -- 1. Register with SDEK (assuming this function returns Either SdekError ...)
  sdekAcceptance <- wrap (Sdek.registerOrder (Sdek.buildMinimalOderRequest orCustomerFullName orCustomerPhone)) SdekRegistrationFailed
  orderId_ <- liftIO generateOrderId
  return defOrderConfirmationDetails { orderId = orderId_}


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