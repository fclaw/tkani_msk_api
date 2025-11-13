{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.PutNewOrder(handler) where

import Katip (logTM, Severity(..))

import API.Types (OrderRequest (..), OrderResponse (..), ApiResponse)
import Types (AppM)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: OrderRequest -> AppM (ApiResponse OrderResponse)
handler newOrderRequest_ = do
  -- 1. Log the incoming request
  $(logTM) DebugS "Request received for creating a new order"
  return $ Right OrderResponse { orDescription = mempty, orTotalSum = 0.0 }