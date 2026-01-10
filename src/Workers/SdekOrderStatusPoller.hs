{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workers.SdekOrderStatusPoller (orderStatusPoller) where

import Control.Monad (when)
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
import Infrastructure.Services.Sdek.Types.OrderInTransit (SdekShipmentState (..), respEntity, entityCdekStatus, entityKeepFreeUntil)
import Concurrency (pooledForConcurrentlyN)
import Infrastructure.Utils.Http (handleWorkerApiResponse)
import TH.Location (currentModule)
import Infrastructure.Utils.Http (HttpError (..))
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Utils.Telegram.Markdown (escapeMarkdownV2)


orderStatusPoller :: AppM ()
orderStatusPoller = do
  -- Run the core logic within our application's monad to get access to the DB, logger, etc.
  $(logTM) InfoS "Polling for SDEK order statuses..."
  pool <- fmap _appDBPool ask
  let requiredStatuses = [Registered, Paid, OnRoute, Delivered, PickedUpByCourier]
  eUuids <- getOrdersInTransit requiredStatuses pool
  for_ eUuids $ \uuids ->
    void $ pooledForConcurrentlyN 5 uuids $ \(orderId, uuid, status) -> do 
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
                $(logTM) InfoS $ ls $ 
                  "order " <> 
                  orderId <> 
                  " has not changed status, status: " <> 
                  pack (show status) <> 
                  ", SDEK status: " <> 
                  pack (show (entityCdekStatus entity))
              else 
                $(logTM) InfoS $ ls $ 
                  "order " <> 
                  orderId <> 
                  " has changed status from " <> 
                  pack (show status) <> " to " <> 
                  pack (show newStatus)
              let keepUntil | newStatus == Delivered = 
                              entityKeepFreeUntil entity
                            | otherwise = Nothing
              void $ updateOrderStatus orderId newStatus keepUntil pool)

  when(isLeft eUuids) $ $(logTM) ErrorS $ ls $ "Polling for SDEK order statuses, error " <> fromLeft undefined eUuids

handleSdekFailure :: Text -> UUID -> HttpError -> AppM ()
handleSdekFailure _ _ (JsonDecodeError err) = $(logTM) ErrorS $ ls $ "aeson error " <> err
handleSdekFailure orderId uuid (NetworkError ex) = 
  case fromException @HttpException ex of
    Just (HttpExceptionRequest _ (StatusCodeException response body)) -> do
      let code = statusCode (responseStatus response)
      -- SCENARIO A: FATAL ERROR (400 Bad Request)
      -- SDEK says: "I don't know this UUID".
      if code == 400 || 
         code == 404 then do
         $(logTM) ErrorS $ ls $ "SDEK UUID " <> pack (show uuid) <> " is invalid or deleted. Stopping tracking."
         pool <- fmap _appDBPool ask
         ePair <- markOrderAsInvalid orderId uuid pool
         for_ ePair $ \(msgId, trackN) -> do
          let msgData = HM.fromList [("orderNumber", orderId), ("trackingNumber", trackN)]
          message <- fmap escapeMarkdownV2 $ render $currentModule msgData
          void $ sendOrEditTelegramMessage mempty message ORDER Nothing (Just msgId) Nothing
      -- SCENARIO B: SERVER ERROR (500, 502)
      -- SDEK is down. Do NOTHING to DB. Just log and wait for next poll.
      else
        $(logTM) WarningS $ ls $ "SDEK Server Error (" <> pack (show code) <> "). Will retry next loop."
    _ -> $(logTM) WarningS $ "SDEK Network Fail. Will retry next loop."


  -- | Logic to map SDEK state (which might be missing) to your Internal Status.
--   We treat 'Nothing' (missing field) exactly like 'StatusCreated'.
mapSdekToInternal :: Maybe SdekShipmentState -> OrderStatus -> OrderStatus
-- 1. If CDEK data is missing, keep existing status
mapSdekToInternal Nothing current = current

-- 2. If the order is already final (Completed/Cancelled), ignore webhooks 
--    (prevents accidental reopening if delayed webhooks arrive)
mapSdekToInternal (Just _) current 
  | current == Completed || current == Cancelled = current

mapSdekToInternal (Just sdekState) current = case sdekState of
  -- ==========================================================
  -- A. PRE-TRANSIT
  -- ==========================================================
  -- Don't downgrade if already Paid or moving
  StatusCreated                           -> if current > Registered 
                                             then current 
                                             else Registered

  StatusRemoved                           -> Cancelled

  -- ==========================================================
  -- B. ACTIVE TRANSIT (Any physical movement = OnRoute)
  -- ==========================================================
  -- Sender City Processing
  StatusAccepted                          -> OnRoute
  StatusReceivedAtShipmentWarehouse       -> OnRoute
  StatusReadyForShipmentInSenderCity      -> OnRoute
  StatusTakenByTransporterFromSenderCity  -> OnRoute
  StatusReadyToShipAtSendingOffice        -> OnRoute
  StatusReadyForShipmentInTransitCity     -> OnRoute
  StatusReturnedToSenderCityWarehouse     -> OnRoute
  StatusReturnedToRecipientCityWarehouse  -> OnRoute
  
  -- Between Cities
  StatusSentToTransitCity                 -> OnRoute
  StatusAcceptedInTransitCity             -> OnRoute
  StatusSentToRecipientCity               -> OnRoute
  
  -- Destination City / Last Mile
  StatusAcceptedAtDeliveryWarehouse       -> OnRoute
  StatusTakenByCourier                    -> OnRoute

  -- ==========================================================
  -- C. FINAL STAGES
  -- ==========================================================
  StatusPostomatPosted                    -> Delivered
  StatusPostomatReceived                  -> Completed
  StatusAcceptedAtPickUpPoint             -> Delivered
  StatusDelivered                         -> Completed

  StatusNotDelivered                      -> Cancelled 

  -- Return to sender essentially cancels the sale.
  StatusReturned                          -> Cancelled

  -- ==========================================================
  -- D. UNKNOWN / FALLBACK
  -- ==========================================================
  StatusUnknown _                         -> current       