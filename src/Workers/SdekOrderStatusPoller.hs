{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Workers.SdekOrderStatusPoller (orderStatusPoller) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Katip
import Control.Monad.IO.Class (liftIO)

import App (AppM)

orderStatusPoller :: AppM ()
orderStatusPoller = forever $ do
  -- Run the core logic within our application's monad to get access to the DB, logger, etc.
  $(logTM) InfoS "Polling for SDEK order statuses..."
  liftIO $ threadDelay (5 * 60 * 1000000)