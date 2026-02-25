{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Workers.SimpleOrderOrchestrator (runSimpleOrderOrchestrator) where

import Katip (logTM, Severity(..))
import Control.Monad (forever, void)
import Control.Monad.State.Class (get)
import Control.Concurrent.Async.Lifted (async)


import API.Types (OrderRequest)
import App (AppM, readTVarIO, readTChanIO, _simpleOrdersChan)


runSimpleOrderOrchestrator :: AppM ()
runSimpleOrderOrchestrator = do
  $(logTM) InfoS "Simple Order Orchestrator started."
  stVar <- get
  st <- readTVarIO stVar
  let inChan = _simpleOrdersChan st
  forever $ do
    -- Block and wait for a new order to appear in the channel
    order <- readTChanIO inChan
    void $ async $ orchestrateSingleOrder order

orchestrateSingleOrder :: OrderRequest -> AppM ()
orchestrateSingleOrder _ = undefined