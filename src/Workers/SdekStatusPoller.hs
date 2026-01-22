{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Workers.SdekStatusPoller (runSdekStatusPoller) where

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
import App (AppM, _appSdekChan, SdekJob (..))
import Infrastructure.Services.Sdek as Sdek
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
runSdekStatusPoller :: AppM ()
runSdekStatusPoller = do
  $(logTM) InfoS "SDEK Order Status Poller started."
  stVar <- get
  st <- liftIO $ atomically $ readTVar stVar
  let inChan = _appSdekChan st
  forever $ do
    -- 1. Block and wait for a new UUID to appear in the channel
    SdekJob {sjSdekUuid, sjReplyVar} <- liftIO $ atomically $ readTChan inChan
    $(logTM) InfoS $ ls $ "Received job for SDEK poller UUID: " <> show sjSdekUuid
    -- 2. Fork a separate, non-blocking thread for this specific UUID
    void $ async $ do
      let action = pollForSingleOrder sjSdekUuid
      pollerRes <- 
        fmap (join . maybeToEither) $
          timeout thirtySeconds action
      liftIO $ atomically $ putTMVar sjReplyVar pollerRes

-- | Polls SDEK for the status of a single order identified by its UUID.
pollForSingleOrder :: UUID -> AppM (Either Text Text)
pollForSingleOrder sdekUuid = do
  eOrderRes <- Sdek.getOrderStatus sdekUuid
  case eOrderRes of
    Right resp -> do
      case Sdek.sosrRequests resp of
        [] -> do
          let errMsg = 
               Sdek.SdekError 
                Sdek.unexpected_response $ 
                  "SDEK status response did not contain " <> 
                  UUID.toText sdekUuid
          log ErrorS $ ": " <> Sdek.seMessage errMsg
          pure $ Left $ tshow [errMsg]
        (reqStatus : _) ->
          case Sdek.spsState
               reqStatus of
            Sdek.Accepted -> do
              -- The order is still processing. Wait and recurse.
              log DebugS $ ": Status is still ACCEPTED. Retrying..."
              threadDelay (3 * 1000000) -- Wait 3 seconds
              pollForSingleOrder sdekUuid
            Sdek.Invalid -> do
              -- FINAL STATE: SDEK rejected the order.
              let errors = Sdek.spsErrors reqStatus
              log WarningS $ " resulted in INVALID state. Errors: " <> tshow errors
              -- Return the error result, which stops the loop.
              pure $ Left $ tshow errors
            Sdek.Successful -> do
              -- FINAL STATE: SDEK accepted the order!
              let trackingNumber = fromJust $ Sdek.sosrCdekNumber resp -- As you noted
              log InfoS $ " resulted in SUCCESSFUL state. Tracking number: " <> trackingNumber
              pure $ Right trackingNumber
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