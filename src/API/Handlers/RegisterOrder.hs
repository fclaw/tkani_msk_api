{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.RegisterOrder(handler) where

import Katip (logTM, Severity(..))
import Control.Monad.State.Class (get)

import API.Types (OrderRequest, ApiResponse)
import App (AppM, readTVarIO, writeTChanIO, _simpleOrdersChan)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: OrderRequest -> AppM (ApiResponse ())
handler order = do
  $(logTM) InfoS "Request received for creating a new order"
  stVar <- get
  st <- readTVarIO stVar
  let inChan = _simpleOrdersChan st
  fmap Right $ writeTChanIO inChan order