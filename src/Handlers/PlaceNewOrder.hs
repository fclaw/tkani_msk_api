{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell  #-}

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
import Control.Monad (join)
import Control.Applicative (asum)
import Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict as HM


import API.Types (OrderRequest (..), OrderConfirmationDetails (..), ApiResponse, defOrderConfirmationDetails, formatStatus, OrderStatus (New), mkError)
import App (AppM, currentTime, render)
import Infrastructure.Utils.OrderId (generateOrderId)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, MessageIdResponse)
import TH.Location (currentModule)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: OrderRequest -> AppM (ApiResponse OrderConfirmationDetails)
handler newOrderRequest_ = do
  liftIO $ print $(currentModule)
  -- 1. Log the incoming request
  $(logTM) DebugS "Request received for creating a new order"
  orderId_ <- liftIO $ generateOrderId
  tm <- currentTime
  tz <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime tz tm
  -- Automatically finds and renders 'templates/Handlers/PlaceNewOrder.tpl'
  messageText <- render $currentModule $ buildTemplateData newOrderRequest_ localTime orderId_
  eResult <- fmap (first (T.pack . show)) $ sendOrEditTelegramMessage ("new order: " <> orderId_) messageText Nothing
  resp <- for eResult $ \messageId -> do
    -- Log the success for visibility in your main logs.
    $(logTM) InfoS $ "Successfully sent new order notification for " <> logStr orderId_
    $(logTM) InfoS $ "Telegram message id " <> logStr (show messageId)
    return $ Right defOrderConfirmationDetails { orderId = orderId_, paymentLink = "https://www.google.com" }
  return $ first mkError $ join resp


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