{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workers.SdekOrderStatusPoller (orderStatusPoller) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Katip
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.UUID (UUID)
import Data.Either (isLeft, fromLeft)
import Data.Foldable (for_)
import Control.Monad (void)
import Data.Text (Text, pack)
import Control.Exception (fromException)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent( StatusCodeException ), responseStatus)
import Network.HTTP.Types.Status (statusCode, status400)
import qualified Data.HashMap.Strict as HM

import App (AppM, _appDBPool, render, ChatKey (..))
import API.Types (OrderStatus (..))
import Infrastructure.Database (getOrdersInTransit, updateOrderStatus, markOrderAsInvalid)
import qualified Infrastructure.Services.Sdek as Sdek
import Infrastructure.Services.Sdek.Types.OrderInTransit (SdekShipmentState (..), respEntity, entityCdekStatus)
import Concurrency (pooledForConcurrentlyN)
import Infrastructure.Utils.Http (handleWorkerApiResponse)
import TH.Location (currentModule)
import Infrastructure.Utils.Http (HttpError (..))
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)


orderStatusPoller :: AppM ()
orderStatusPoller = forever $ do
  -- Run the core logic within our application's monad to get access to the DB, logger, etc.
  $(logTM) InfoS "Polling for SDEK order statuses..."
  pool <- fmap _appDBPool ask
  eUuids <- liftIO $ getOrdersInTransit [Registered, Paid, OnRoute, Delivered] pool
  for_ eUuids $ \uuids ->
    void $ pooledForConcurrentlyN 3 uuids $ \(orderId, uuid, status) -> do 
      $(logTM) InfoS $ ls $ "requesting status for: " <> show uuid
      eRes <- Sdek.getOrdersInTransit uuid
      handleWorkerApiResponse $(currentModule) eRes
        -- ON ERROR (The complex part)
        (\ex -> handleSdekFailure orderId uuid ex)
        (\res ->
            for_ (respEntity res) $ \entity -> do
              let newStatus = mapSdekToInternal (entityCdekStatus entity) status
              if newStatus == status
              then 
                $(logTM) InfoS $ ls $ "order " <> orderId <> " has not changed status, status: " <> pack (show status)
              else 
                $(logTM) InfoS $ ls $ "order " <> orderId <> " has changed status from " <> pack (show status) <> " to " <> pack (show newStatus)
              void $ liftIO $ updateOrderStatus orderId newStatus pool)

  when(isLeft eUuids) $ $(logTM) ErrorS $ ls $ "Polling for SDEK order statuses, error " <> fromLeft undefined eUuids
  liftIO $ threadDelay (5 * 60 * 1000000)

handleSdekFailure :: Text -> UUID -> HttpError -> AppM ()
handleSdekFailure _ _ (JsonDecodeError err) = $(logTM) ErrorS $ ls $ "aeson error " <> err
handleSdekFailure orderId uuid (NetworkError ex) = 
  case fromException @HttpException ex of
    Just (HttpExceptionRequest _ (StatusCodeException response body)) -> do
      let code = statusCode (responseStatus response)
      -- SCENARIO A: FATAL ERROR (400 Bad Request)
      -- SDEK says: "I don't know this UUID".
      if code == 400 then do
         $(logTM) ErrorS $ ls $ "SDEK UUID " <> pack (show uuid) <> " is invalid or deleted. Stopping tracking."
         pool <- fmap _appDBPool ask
         ePair <- liftIO $ markOrderAsInvalid orderId uuid pool
         for_ ePair $ \(msgId, trackN) -> do
          let msgData = HM.fromList [("orderNumber", orderId), ("trackingNumber", trackN)]
          message <- render $currentModule msgData
          void $ sendOrEditTelegramMessage mempty message ORDER Nothing (Just msgId)
      -- SCENARIO B: SERVER ERROR (500, 502)
      -- SDEK is down. Do NOTHING to DB. Just log and wait for next poll.
      else
        $(logTM) WarningS $ ls $ "SDEK Server Error (" <> pack (show code) <> "). Will retry next loop."
    _ -> $(logTM) WarningS $ "SDEK Network Fail. Will retry next loop."


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