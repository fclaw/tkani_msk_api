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

-- | Universal mapping with state-drift protection
-- | Prevents older API statuses from overwriting newer local stages.
mapYandexToInternal :: YandexOrderStatus -> OrderStatus -> OrderStatus
mapYandexToInternal yandex current =
    let 
        -- Helper: Determine what the Yandex status suggests the NEW status should be
        proposed = case yandex of
            -- STAGE A: PRE-TRANSIT
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

            -- STAGE B: PICKED UP (At Source SC)
            SortingCenterAtStart           -> PickedUpByCourier
            SortingCenterPrepared          -> PickedUpByCourier

            -- STAGE C: ACTIVE TRANSIT (In network or at PVZ)
            SortingCenterTransmitted       -> OnRoute
            DeliveryAtStart                -> OnRoute
            DeliveryTransportation         -> OnRoute
            DeliveryArrivedPickupPoint     -> OnRoute
            DeliveryStoragePeriodExtended  -> OnRoute

            -- STAGE D: FINAL STAGES
            DeliveryTransmittedToRecipient -> Delivered
            ConfirmationCodeReceived       -> Delivered
            ParticularlyDelivered          -> Delivered
            DeliveryDelivered              -> Delivered
            Finished                       -> Completed
            DeliveryStoragePeriodExpired   -> Cancelled -- Unclaimed return
    in
        -- Decision Logic: Only update if the 'proposed' state is "Ahead" 
        -- or "Terminal" (Cancelled/Completed) compared to the 'current' state.
        if isNewer proposed current then proposed else current

-- | Helper to define the "Ranking" or priority of statuses.
-- | A status is "Newer" if it is further down the logistics chain.
isNewer :: OrderStatus -> OrderStatus -> Bool
isNewer proposed current = 
    rank proposed > rank current
  where
    rank :: OrderStatus -> Int
    rank = \case
        -- [0] Pre-payment / Intent phase
        Registered          -> 0 
        
        -- [1] Business Goal achieved: Payment received
        -- Fulfillment begins here.
        Paid                -> 1 

        -- [2] Warehouse / Logistics Prep
        AddedToPickupQueue  -> 2
        ScheduledForPickup  -> 3

        -- [3] Out for Delivery (Physically moved from warehouse)
        PickedUpByCourier   -> 4
        OnRoute             -> 5

        -- [4] arrival at pickup point
        Delivered           -> 6
        
        -- [5] Terminal States
        Completed           -> 7
        Cancelled           -> 7