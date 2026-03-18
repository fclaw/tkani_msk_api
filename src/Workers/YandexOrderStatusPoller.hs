{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workers.YandexOrderStatusPoller (runYandexOrderStatusPoller) where


import Data.Foldable (for_)
import Control.Monad (void, when)
import Data.Either (isLeft, fromLeft)
import Katip (logTM, Severity(..), ls)
import Control.Monad.Reader.Class (ask)


import Text (tshow)
import App (AppM, _appDBPool)
import API.Types (OrderStatus (..))
import Concurrency (pooledForConcurrentlyN)
import Infrastructure.Database (getYandexOrdersInTransit, updateYandexOrderStatus)
import Infrastructure.Services.Yandex.Types (OrderParticulars (..), osStatus)
import qualified Infrastructure.Services.Yandex.Types as YA
import Infrastructure.Services.Yandex.Types.Enums (YandexOrderStatus (..))
import Infrastructure.Services.Yandex (fetchOrderParticulars)


runYandexOrderStatusPoller :: AppM ()
runYandexOrderStatusPoller = do
 -- Run the core logic within our application's monad to get access to the DB, logger, etc.
  $(logTM) InfoS "Polling for YANDEX order statuses..."
  pool <- fmap _appDBPool ask
  let requiredStatuses = 
       [ Registered
       , Paid
       , OnRoute
       , Delivered
       , PickedUpByCourier
       , ScheduledForPickup
       , AddedToPickupQueue
       , PickupFailed
       ]
  eDbRes <- getYandexOrdersInTransit requiredStatuses pool
  for_ eDbRes $ \xs ->
    void $ pooledForConcurrentlyN 5 xs $ 
      \(orderId, yandexOrderId, requestId, status) -> do 
        $(logTM) InfoS $ ls $ "requesting status for: " <> requestId
        OrderParticulars {state=yaState@YA.OrderStatus{osStatus=yaStatus}} <- fetchOrderParticulars requestId
        let newStatus = mapYandexToInternal yaStatus status
        if newStatus == status
        then 
          $(logTM) InfoS $ ls $ 
            "order " <> 
            orderId <> 
            " has not changed status, status: " <> 
            (tshow status) <> 
            ", YANDEX status: " <>
            (tshow yaStatus)
        else do
          $(logTM) InfoS $ ls $ 
            "order " <> 
            orderId <> 
            " has changed status from " <> 
            (tshow status) <> " to " <> 
            (tshow newStatus) <>
            ", YANDEX status: " <> 
            (tshow yaStatus)
          pool <- fmap _appDBPool ask
          eDbRes <- updateYandexOrderStatus orderId yandexOrderId newStatus yaState pool
          when (isLeft eDbRes) $ $(logTM) ErrorS $  "failed to update YANDEX order status, error: " <> ls (fromLeft undefined eDbRes)

  when(isLeft eDbRes) $ $(logTM) ErrorS $ ls $ "Polling for YANDEX order statuses, error " <> fromLeft undefined eDbRes


-- | Updated mapping based on the latest Yandex Status definitions.
mapYandexToInternal :: YandexOrderStatus -> OrderStatus -> OrderStatus
mapYandexToInternal yandex current =
    let proposed = case yandex of
            -- =================================================================
            -- STAGE A: PRE-TRANSIT
            -- =================================================================
            Draft                          -> Registered
            Validating                     -> Registered
            Created                        -> Registered
            DeliveryProcessingStarted      -> Registered
            DeliveryTrackReceived          -> Registered
            ValidatingError                -> Cancelled
            SortingCenterProcessingStarted -> AddedToPickupQueue
            SortingCenterTrackReceived     -> AddedToPickupQueue
            SortingCenterTrackLoaded       -> AddedToPickupQueue
            DeliveryLoaded                 -> ScheduledForPickup
            SortingCenterLoaded            -> ScheduledForPickup

            -- =================================================================
            -- STAGE B: PICKED UP (Warehouse to Sender's Hub)
            -- =================================================================
            SortingCenterAtStart           -> PickedUpByCourier
            SortingCenterPrepared          -> PickedUpByCourier

            -- =================================================================
            -- STAGE C: ACTIVE TRANSIT (Between Hubs and to Customer)
            -- =================================================================
            SortingCenterTransmitted       -> OnRoute
            DeliveryAtStart                -> OnRoute
            DeliveryAtStartSort            -> OnRoute  -- Destination city sorting
            DeliveryTransportationRecipient -> OnRoute -- Courier on the way
            DeliveryAttemptFailed          -> PickupFailed -- UI needs to know about the failure

            -- =================================================================
            -- STAGE D: FINAL STAGES (Handover and Closure)
            -- =================================================================
            DeliveryTransmittedToRecipient -> Delivered
            DeliveryDelivered              -> Delivered
    in
        if isNewer proposed current then proposed else current

-- | Ranking logic updated to handle specific delivery failures.
isNewer :: OrderStatus -> OrderStatus -> Bool
isNewer proposed current = 
    rank proposed > rank current
  where
    rank :: OrderStatus -> Int
    rank = \case
        -- [0] Registration
        Registered          -> 0 
        
        -- [1] User paid for goods
        Paid                -> 1 

        -- [2] Order being packed/batched
        AddedToPickupQueue  -> 2
        ScheduledForPickup  -> 3

        -- [3] Physically departed warehouse
        PickedUpByCourier   -> 4
        
        -- [4] Logistics grid movement
        OnRoute             -> 5
        PickupFailed        -> 5 -- Note: Attempt failed, still considered "active/on route" rank

        -- [5] Arrived at destination / Point
        Delivered           -> 6
        
        -- [6] Reached terminal outcome
        Completed           -> 7
        Cancelled           -> 7