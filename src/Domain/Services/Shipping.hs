{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Domain.Services.Shipping (prepareAndSchedulePickup) where


import Data.Text (pack)
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay, Day)
import Data.UUID (UUID)
import Control.Monad.IO.Class (liftIO)
import Katip (logTM, Severity(..), ls)
import Control.Monad.Reader.Class (ask)
import Data.Foldable (for_)
import Control.Monad (unless)
import Data.Time.Calendar (addDays)
import Control.Monad (when, forM_, forM)
import Data.Text as T (intercalate)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Either (isRight, isLeft, partitionEithers, fromLeft)


import Text (tshow, encodeToText)
import App (AppM, _appDBPool, _sdekConfig)
import Concurrency (pooledForConcurrentlyN)
import Infrastructure.Services.Sdek.Types.State
import Infrastructure.Services.Sdek  (scheduleSingleOrderCourier)
import Infrastructure.Services.Sdek.Types.Error (SdekErrorDetail (..))
import Infrastructure.Services.Sdek.Types.Config (SdekConfig (..))
import Infrastructure.Services.Sdek.Types.Courier (SdekCourierResponse (..), SdekRequestDto (..), uuid)
import Infrastructure.Database (pickupOrdersForShipment, recordCourierPickupFailure, createCourierPickupPromise)




prepareAndSchedulePickup :: AppM Bool
prepareAndSchedulePickup = do
    $(logTM) InfoS "Checking for paid orders to schedule for pickup..."
    -- Get the current date to pass to the query for the idempotency check
    today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
    -- 1. Atomically find and update the orders.
    --    The query now has built-in guards.
    cfg <- ask
    let pool = _appDBPool cfg
    let minCourierPickup = pickupMinimum . _sdekConfig $ cfg
    let status = convertStateToSql Successful
    eOrdersToSchedule <- pickupOrdersForShipment status pool
    case eOrdersToSchedule of
      Left dbErr -> fmap (const False) $ $(logTM) ErrorS $ ls $ "DB error while fetching paid orders: " <> tshow dbErr
      Right orders ->
        if null orders then
          fmap (const False) $ $(logTM) InfoS $ "No new paid orders to schedule."
        else if length orders < minCourierPickup then
          fmap (const False) $ 
            $(logTM) InfoS $ ls $
              "Found only " <> tshow (length orders) <>
              " orders, which is below the minimum threshold of " <>
              tshow minCourierPickup <> 
              " for courier pickup. Skipping scheduling."
        else do
          -- We have enough orders to schedule a pickup
          $(logTM) InfoS "Scheduling courier pickup for orders..."          
          -- ... (the rest of your logic to call the SDEK API) ...
          $(logTM) InfoS $ ls $ "Found " <> tshow (length orders) <> " orders. Scheduling courier..."
          -- 2. For each order we just claimed, call the SDEK API
          --    We can run these in parallel with a bounded concurrency.
          results <- pooledForConcurrentlyN 3 orders scheduleSingleOrderCourier
          -- Let's separate the successes from the failures first for clarity.
          let (failures, successes) = partitionEithers results
          -- Check if any of the API calls failed
          -- 1. Handle any hard network failures
          forM_ failures $ \err ->
            $(logTM) ErrorS $ ls $ "A SDEK courier call request failed at the network level: " <> tshow err
          fmap (const True) $ forM_ successes $ \(orderId, SdekCourierResponse {entity, requests}) -> do 
            records <- forM requests $ \SdekRequestDto {..} -> do
              -- A. Check if the request was accepted or failed validation
              let entityUuid = uuid entity
              if state == Invalid
              then do
                -- THE REQUEST FAILED VALIDATION ON SDEK'S SIDE
                let errorDetails = fromMaybe [] errors
                let errorMsg = T.intercalate ", " (map message errorDetails)
                      
                $(logTM) ErrorS $ ls $ 
                  "SDEK rejected courier call for UUID " <> tshow entityUuid
                  <> ". Status: " <> tshow state
                  <> ". Errors: " <> errorMsg
                      
                -- DB ACTION: Log this failure
                eDbRes <- recordCourierPickupFailure entityUuid errorMsg pool
                when(isLeft eDbRes) $
                  $(logTM) ErrorS $ ls $ 
                  "Failed to record courier pickup failure for SDEK pickup " <>
                  tshow entityUuid <> ": " <>
                  tshow (fromLeft undefined eDbRes)
                return Nothing

              else do
                -- SUCCESS (or waiting). The request was accepted by SDEK.
                $(logTM) InfoS $ ls $
                  "SDEK accepted courier call for UUID " <> tshow entityUuid
                  <> ". Initial status: " <> tshow state
                  <> ". order ID: " <> orderId
                return $ Just (orderId, entityUuid, convertStateToSql state)
            
            let tomorrow = addDays 1 today
            eDbRes <- createCourierPickupPromise (catMaybes records) tomorrow pool
            when(isLeft eDbRes) $ $(logTM) ErrorS $ ls $ "Failed to records courier pickup for SDEK pickup " <> tshow (fromLeft undefined eDbRes)