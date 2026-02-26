{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}

module API.Handlers.Shelf.InitiateShipment (handler) where

import Data.Int (Int64)
import Katip (logTM, Severity(..))
import Control.Monad.State.Class (get)

import API.WithField (WithField)
import API.Types (InitiateShelfShipment, ApiResponse)
import App (AppM, readTVarIO, writeTChanIO, _shelfOrdersChan)



handler :: Int64 -> WithField "chat_id" Int64 InitiateShelfShipment -> AppM (ApiResponse ())
handler userId init = do
  $(logTM) InfoS "Request received for initiating shelf shipment."
  stVar <- get
  st <- readTVarIO stVar
  let inChan = _shelfOrdersChan st
  fmap Right $ writeTChanIO inChan (userId, init)