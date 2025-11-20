{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Workers.TinkoffPaymentStatusPoller (paymentStatusPoller) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Katip
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM
import Control.Monad.State.Class (get)
import Control.Monad.Reader.Class (ask)
import Control.Concurrent.Async.Lifted (async)
import Data.Text (Text, pack)
import Data.Foldable (for_)
import Control.Concurrent (threadDelay)

import App (AppM, runAppM, _tinkoffPaymentChan, _appDBPool, ChatKey (CONCIERGE))
import Infrastructure.Utils.Http (getReq)
import Infrastructure.Services.Tinkoff.Types (PaymentDetails (..))
import Infrastructure.Database (getChatDetails)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)


-- Configuration constants (in microseconds)
delayFast   =  4 * 1000000 -- 4 seconds
delayMedium = 12 * 1000000 -- 12 seconds
delaySlow   = 45 * 1000000 -- 45 seconds

readTVar = liftIO . STM.atomically . STM.readTVar
readTChan = liftIO . STM.atomically . STM.readTChan

paymentStatusPoller :: AppM ()
paymentStatusPoller = do
  -- Run the core logic within our application's monad to get access to the DB, logger, etc.
  $(logTM) InfoS "Polling for Tinkoff payment statuses..."
  stvar <- get
  st <- readTVar stvar
  forever $ readTChan (_tinkoffPaymentChan st) >>= (void . async . workerLogic)

workerLogic :: PaymentDetails -> AppM ()
workerLogic PaymentDetails {..} = do
  liftIO $ threadDelay (30 * 1000000)
  $(logTM) InfoS $ ls $ "Processing payment id: " <> paymentId
  cfg <- ask
  let pool = _appDBPool cfg
  eResult <- liftIO $ getChatDetails orderId pool
  case eResult of
    Right mDetails -> do
      liftIO $ print (orderId, mDetails)
      for_ mDetails $ \messageId -> 
        void $ sendOrEditTelegramMessage mempty "test" CONCIERGE (Just messageId)
    Left err -> $(logTM) ErrorS $ ls $ "error in fetching message details: " <> err