{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Workers.SdekCourierStatusPoller (runSdekCourierStatusPoller) where

import Katip
import Control.Concurrent.STM (atomically, readTChan, readTVar, writeTChan)
import Control.Monad (forever, void, join)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async.Lifted (async)
import Control.Monad.State.Class (get)
import Data.UUID (UUID)
import Data.Text (Text)
import System.Timeout.Lifted (timeout)
import Control.Concurrent.Lifted (threadDelay)
import qualified Data.UUID as UUID
import Data.Maybe (fromJust)
import Control.Concurrent.STM.TMVar (putTMVar)


import Text (tshow)
import App (AppM, _sdekCourierChan, SdekCourierJob (..), readTVarIO, writeTChanIO, readTChanIO)
import qualified Infrastructure.Services.Sdek as Sdek
import Infrastructure.Services.Sdek.Types.State (SdekRequestState)
import qualified Infrastructure.Services.Sdek.Types.Courier as Courier
import qualified Infrastructure.Services.Sdek.Types as Sdek
import qualified Infrastructure.Services.Sdek.Types.State as Sdek


sec :: Int
sec = 1000000

thirtySeconds :: Int
thirtySeconds = 30 * sec

maybeToEither :: Maybe a -> Either Text a
maybeToEither (Just v) = Right v
maybeToEither Nothing = Left "Sdek timeout exceeded"


-- | The main dispatcher loop for the poller.
--   It reads UUIDs from a channel and forks a worker for each one.
runSdekCourierStatusPoller :: AppM ()
runSdekCourierStatusPoller = do
  $(logTM) InfoS "SDEK Courier Status Poller started."
  stVar <- get
  st <- readTVarIO stVar
  let inChan = _sdekCourierChan st
  forever $ do
    -- 1. Block and wait for a new UUID to appear in the channel
    SdekCourierJob {scjSdekUuid, scjReplyVar} <- readTChanIO inChan
    $(logTM) InfoS $ ls $ "Received job for SDEK poller UUID: " <> show scjSdekUuid
    -- 2. Fork a separate, non-blocking thread for this specific UUID
    void $ async $ do
      let action = pollForSingleCourier scjSdekUuid
      pollerRes <- 
        fmap (join . maybeToEither) $
          timeout thirtySeconds action
      liftIO $ atomically $ putTMVar scjReplyVar pollerRes

-- | Polls SDEK for the status of a single order identified by its UUID.
pollForSingleCourier :: UUID -> AppM (Either Text (SdekRequestState, Courier.SdekPickupAppStatus))
pollForSingleCourier sdekUuid = do
  eAppResp <- Sdek.getPickupApplication sdekUuid
  case eAppResp of
    Right resp ->
      case Courier.sparState resp of
        Sdek.Accepted -> do
         -- The order is still processing. Wait and recurse.
          log DebugS $ ": Status is still ACCEPTED. Retrying..."
          threadDelay (3 * 1000000) -- Wait 3 seconds
          pollForSingleCourier sdekUuid
        Sdek.Invalid -> do
          -- FINAL STATE: SDEK rejected the order.
          let errors = Courier.sparErrors resp
          log WarningS $ " resulted in INVALID state. Errors: " <> tshow errors
          -- Return the error result, which stops the loop.
          pure $ Right (Sdek.Invalid, Courier.sparStatus resp)
        Sdek.Successful -> do
          -- FINAL STATE: SDEK accepted the courier application!
          log InfoS $ " resulted in SUCCESSFUL state."
          pure $ Right (Sdek.Successful, Courier.sparStatus resp)
        other -> do
          let errMsg = 
               Sdek.SdekError 
                 Sdek.unexpected_response $ 
                  "SDEK returned an unexpected final status: " <> 
                  tshow other
          log ErrorS $ ": " <> Sdek.seMessage errMsg
          pure $ Left $ tshow [errMsg]
    Left err -> pure $ Left $ tshow err
  where log severity msg = $(logTM) severity $ logStr $ "Polling " <> UUID.toText sdekUuid <> msg