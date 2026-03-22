{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.Yandex.FinalizeShipment (handler) where

import Katip (logTM, Severity(..))
import Control.Monad.State.Class (get)

import App (AppM, readTVarIO, _shipmentChan, writeTChanIO)
import API.Types (ApiResponse, YandexShipmentFinalizeReq)


handler :: YandexShipmentFinalizeReq -> AppM (ApiResponse ())
handler req = do
  stVar <- get
  st <- readTVarIO stVar
  let inChan = _shipmentChan st
  fmap Right $ writeTChanIO inChan req