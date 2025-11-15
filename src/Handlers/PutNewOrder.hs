{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.PutNewOrder(handler) where

import Katip (logTM, Severity(..))

import API.Types (OrderRequest (..), OrderConfirmationDetails (..), ApiResponse, defOrderConfirmationDetails)
import Types (AppM)
import Infrastructure.Utils.OrderId (generateOrderId)
import Control.Monad.IO.Class (liftIO)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: OrderRequest -> AppM (ApiResponse OrderConfirmationDetails)
handler newOrderRequest_ = do
  -- 1. Log the incoming request
  $(logTM) DebugS "Request received for creating a new order"
  orderId_ <- liftIO $ generateOrderId
  return $ Right defOrderConfirmationDetails { orderId = orderId_, paymentLink = "https://www.google.com", totalAmount = 213.89 }