{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Workers.YandexOrderStatusPoller (runYandexOrderStatusPoller) where


import Data.Text (Text, pack)
import Data.Foldable (for_)
import Control.Monad (void, when)
import Data.Either (isLeft, fromLeft)
import Katip (logTM, Severity(..), ls)
import Control.Monad.Reader.Class (ask)
-- retry imports
import Control.Monad.IO.Class    (liftIO)
import Control.Exception.Lifted  (throwIO, try)
import qualified Network.Wreq    as Wreq
import Network.HTTP.Client       (HttpException(..), HttpExceptionContent(..), responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Control.Concurrent        (threadDelay)
import Control.Lens              ((^.))
import Control.Exception         (Exception)
import GHC.Generics              (Generic)


import Text (tshow)
import App (AppM, _appDBPool)
import API.Types (OrderStatus (..))
import Concurrency (pooledForConcurrentlyN, runJobWithCleanup)
import Infrastructure.Database (getYandexOrdersInTransit, updateYandexOrderStatus)
import Infrastructure.Services.Yandex.Types (OrderParticulars (..), osStatus)
import qualified Infrastructure.Services.Yandex.Types as YA
import Infrastructure.Services.Yandex.Types.Enums (YandexOrderStatus (..), YandexStatusCode (..))
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
    void $ pooledForConcurrentlyN 1 xs $ 
      \(orderId, yandexOrderId, requestId, status) ->
        runJobWithCleanup $
          withYaRetry 5 $ do
            $(logTM) InfoS $ ls $ "requesting status for: " <> requestId
            OrderParticulars {state=yaState@YA.OrderStatus{osStatus=yaStatus}} <- fetchOrderParticulars requestId
            let newStatus = mapYandexToInternal yaStatus status
            if newStatus == status
            then logStatusNoChange orderId status yaStatus
            else do 
              logStatusChange orderId status newStatus yaStatus
              pool <- fmap _appDBPool ask
              eDbRes <- updateYandexOrderStatus orderId yandexOrderId newStatus yaState pool
              when (isLeft eDbRes) $ $(logTM) ErrorS $  "failed to update YANDEX order status, error: " <> ls (fromLeft undefined eDbRes)

  when(isLeft eDbRes) $ $(logTM) ErrorS $ ls $ "Polling for YANDEX order statuses, error " <> fromLeft undefined eDbRes


logStatusChange :: Text -> OrderStatus -> OrderStatus -> YandexOrderStatus -> AppM ()
logStatusChange orderId oldStatus newStatus yaStatus = 
  $(logTM) InfoS $ ls $ 
    "order " <> orderId <> " has changed status from " <> 
    (tshow oldStatus) <> " to " <> (tshow newStatus) <> 
    ", YANDEX status: " <> (tshow yaStatus)

logStatusNoChange :: Text -> OrderStatus -> YandexOrderStatus -> AppM ()
logStatusNoChange orderId status yaStatus =
  $(logTM) InfoS $ ls $
    "order " <> orderId <> " has not changed status, status: " <>
    (tshow status) <> ", YANDEX status: " <> (tshow yaStatus)

mapYandexToInternal :: YandexOrderStatus -> OrderStatus -> OrderStatus
mapYandexToInternal (UnknownStatus _) current =
  -- SAFE: If we don't know what the tag means, don't change the local state.
  -- However, you should still append 'rawTag' to your DB history log! 
  current
mapYandexToInternal (KnownStatus yandex) current =
    let proposed = case yandex of
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

            -- STAGE B: PICKED UP
            SortingCenterAtStart           -> PickedUpByCourier
            SortingCenterPrepared          -> PickedUpByCourier

            -- STAGE C: ACTIVE TRANSIT / MODIFICATIONS
            SortingCenterTransmitted        -> OnRoute
            DeliveryAtStart                 -> OnRoute
            DeliveryAtStartSort             -> OnRoute
            DeliveryTransportationRecipient -> OnRoute
            DeliveryUpdatedByShop           -> OnRoute -- Date changed, but still active
            DeliveryUpdatedByRecipient      -> OnRoute
            DeliveryUpdatedByDelivery       -> OnRoute
            DeliveryAttemptFailed           -> OnRoute

            -- STAGE D: TERMINAL / RETURNS
            -- Physical handover (Trigger the 'Congratulations' message here)
            DeliveryTransmittedToRecipient  -> Delivered
            -- Final Business Outcome (Close the DB record)
            DeliveryDelivered               -> Completed
            
            -- Any Cancellation = Cancelled
            GeneralCancelled                -> Cancelled
            CancelledByRecipient            -> Cancelled
            CancelledUser                   -> Cancelled
            CancelledInPlatform             -> Cancelled
            SortingCenterCancelled          -> Cancelled
            
            -- Any Return status = Cancelled (Logic: sale is dead, parcel returning)
            SortingCenterReturnPreparing       -> Cancelled
            SortingCenterReturnPreparingSender -> Cancelled
            SortingCenterReturnArrived         -> Cancelled
            SortingCenterReturnReturned        -> Cancelled
            ReturnPreparing                    -> Cancelled
            ReturnTransportationStarted        -> Cancelled
            ReturnArrivedDelivery              -> Cancelled
            ReturnTransmittedFulfilment        -> Cancelled
            ReturnReadyForPickup               -> Cancelled
            ReturnReturned                     -> Cancelled
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

-- | and still receive 403 or 429.
data YandexRetryExhausted = YandexRetryExhausted Text
  deriving (Show, Generic, Exception)


-- | A wrapper that retries a specific operation if it hits 429 or 403 errors.
-- Now generalized to return any type 'a'
withYaRetry :: Int -> AppM a -> AppM a
withYaRetry attempt action = do
  result <- try action
  case result of
    Right val -> pure val -- Success!
    
    Left err@(HttpExceptionRequest _ (StatusCodeException resp body)) -> 
      let code = resp ^. Wreq.responseStatus . Wreq.statusCode
      in if code `elem` [403, 429]
         then 
           if attempt >= 5 
           then throwIO $ YandexRetryExhausted (pack $ "Max retries on code " <> show code)
           else do
             -- Logic: 
             -- For 429: 2, 4, 8, 16s...
             -- For 403: 7, 9, 13, 21s... (Extra 5s safety for security blocks)
             let base = if code == 403 then 5 else 0
             let delay = (base + 2 ^ attempt) * 1000000 
            
             liftIO $ threadDelay delay
             withYaRetry (attempt + 1) action
         else throwIO err
    Left err -> throwIO err
