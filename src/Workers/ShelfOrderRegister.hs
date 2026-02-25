{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Workers.ShelfOrderRegister (runShelfOrderRegister) where

import Data.Int (Int64)
import Katip (logTM, Severity(..))
import Control.Monad (forever, void)
import Control.Monad.State.Class (get)
import Control.Concurrent.Async.Lifted (async)


import API.Types (InitiateShelfShipment)
import App (AppM, readTVarIO, readTChanIO, _shelfOrdersChan)


runShelfOrderRegister :: AppM ()
runShelfOrderRegister = do
  $(logTM) InfoS "Shelf Order Register started."
  stVar <- get
  st <- readTVarIO stVar
  let inChan = _shelfOrdersChan st
  forever $ do
    -- Block and wait for a new order to appear in the channel
    order <- readTChanIO inChan
    void $ async $ uncurry runSingleRegister order

runSingleRegister :: Int64 -> InitiateShelfShipment -> AppM ()
runSingleRegister _ _ = undefined