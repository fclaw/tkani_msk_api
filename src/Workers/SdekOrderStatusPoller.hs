{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Workers.SdekOrderStatusPoller (orderStatusPoller) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Katip
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.Async.Lifted as Async
import Control.Monad.Reader.Class (ask)
import Data.UUID (UUID)
import Data.Either (isLeft, fromLeft)
import Data.Foldable (for_)
import Control.Monad (void)
import Data.Text (pack)

import App (AppM, _appDBPool)
import API.Types (OrderStatus (..))
import Infrastructure.Database (getOrdersInTransit, updateOrderStatus)
import qualified Infrastructure.Services.Sdek as Sdek
import Infrastructure.Services.Sdek.Types.OrderInTransit (SdekShipmentState (..), respEntity, entityCdekStatus)


orderStatusPoller :: AppM ()
orderStatusPoller = forever $ do
  -- Run the core logic within our application's monad to get access to the DB, logger, etc.
  $(logTM) InfoS "Polling for SDEK order statuses..."
  pool <- fmap _appDBPool ask
  eUuids <- liftIO $ getOrdersInTransit [Registered, Paid, OnRoute, Delivered] pool
  for_ eUuids $ \uuids ->
    Async.forConcurrently_ uuids $ \(orderId, uuid, status) -> do 
      $(logTM) InfoS $ ls $ "requesting status for: " <> show uuid
      eRes <- Sdek.getOrdersInTransit uuid
      when(isLeft eRes) $ $(logTM) ErrorS $ ls $ "sdek error " <> show (fromLeft undefined eRes)
      for_ eRes $ \res -> 
        for_ (respEntity res) $ \entity -> do
          let newStatus = mapSdekToInternal (entityCdekStatus entity) status
          if newStatus == status 
          then 
            $(logTM) InfoS $ ls $ "order " <> orderId <> " has not changed status, status: " <> pack (show status)
          else 
            $(logTM) InfoS $ ls $ "order " <> orderId <> " has changed status from " <> pack (show status) <> " to " <> pack (show newStatus)
          void $ liftIO $ updateOrderStatus orderId newStatus pool

  when(isLeft eUuids) $ $(logTM) ErrorS $ ls $ "Polling for SDEK order statuses, error " <> fromLeft undefined eUuids
  liftIO $ threadDelay (5 * 60 * 1000000)


  -- | Logic to map SDEK state (which might be missing) to your Internal Status.
--   We treat 'Nothing' (missing field) exactly like 'StatusCreated'.
mapSdekToInternal :: Maybe SdekShipmentState -> OrderStatus -> OrderStatus
mapSdekToInternal mbSdekSt currentInternalStatus = 
  case mbSdekSt of
    -- 1. If field is missing, nothing has happened physically.
    -- Keep the status exactly as it is in the DB (whether Registered or Paid).
    Nothing -> currentInternalStatus

    -- 2. If we have a status, process it
    Just sdekSt -> case sdekSt of
        
        -- Paperwork created, but courier doesn't have the box.
        -- Do not move to 'OnRoute'. Keep current state (e.g. Paid).
        StatusCreated -> currentInternalStatus 

        -- The Courier has scanned the box!
        StatusAccepted -> OnRoute 
        StatusSent     -> OnRoute
        StatusArrived  -> OnRoute -- At sorting center/dest city

        -- The box is at the Pickup Point (PVZ) waiting for client
        StatusReadyForPickup -> Delivered 

        -- The Client has the box
        StatusDelivered -> Completed 

        -- Exceptions
        StatusNotDelivered -> Cancelled -- Or handle manually
        StatusUnknown _    -> currentInternalStatus