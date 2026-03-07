{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Workers.DailyWeightTracker (runDailyWeightTracker) where

import Katip (logTM, ls, Severity (..))
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.String (fromString)
import Control.Monad (void, forever, when)
import Control.Monad.IO.Class (liftIO)
import Servant.Server (ServerError, err500)
import qualified Data.Text as T
import Data.Foldable (for_)
import Data.Either (isLeft, fromLeft)
import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.List (delete)
import Control.Monad.Catch (throwM)
import Data.FileEmbed (embedFile)
import qualified Data.Text.Encoding as TE
import Control.Concurrent.Async (async, waitAnyCatchCancel)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Concurrent (threadDelay)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Encoding (decodeUtf8)
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar, atomically, modifyTVar', TChan, writeTChan)
import Data.Time (Day, UTCTime, getCurrentTime, utctDay)
import qualified Database.PostgreSQL.Simple as PG
import Data.Time.Calendar (dayOfWeek, DayOfWeek(..))
import qualified Database.PostgreSQL.Simple.Notification as PG
import Data.Time.LocalTime (TimeZone (..), getCurrentTimeZone, getZonedTime, zonedTimeToUTC, utcToLocalTime, localTimeOfDay, TimeOfDay (..), localDay)

import App
import API.Types (OrderStatus (ScheduledForPickup, AddedToPickupQueue))
import Text (camelToSnake, tshow, textMoneyToDouble, encodeToText)
import Concurrency (runJobWithCleanup)
import TH.Location (currentModule)
import Infrastructure.Services.Dostavista.Types
import Infrastructure.Services.Dostavista (scheduleDostavistaPickup)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import  Infrastructure.Services.Dostavista.Types.Config (courierCallCutoffHour)
import Infrastructure.Database (fetchDostavistaPackages, updateOrderStatus, recordAndLinkPickup, fetchWeightTrackerStateInfo, CourierService (DOSTAVISTA), CourierPickupData (..))


-- ADT for the notification payload
data WeighedOrderEvent = 
     WeighedOrderEvent 
     { orderId     :: Text
     , weightGrams :: Int
     } deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake } ''WeighedOrderEvent)

-- The in-memory state for our worker
data WeightTrackerState = 
     WeightTrackerState
     { wtsCurrentDay       :: Day
     , wtsTotalWeightGrams :: Int
     , wtsCourierCalled    :: Bool   -- A flag to ensure we only call once per day
     , wtsOrders           :: [Text] -- what orders are actually handed over to a courier in the batch
     } deriving (Show)


runDailyWeightTracker :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runDailyWeightTracker connInfo runAppM = do
  $(logTM) InfoS "Daily Weight Tracker worker started."
  -- 1. Initialize State
  initialDay <- utctDay <$> liftIO getCurrentTime
  pool <- fmap _appDBPool ask
  eDbResult <- fetchWeightTrackerStateInfo initialDay DOSTAVISTA pool
  case eDbResult of
    Left dbErr -> $(logTM) ErrorS $ "fetching info for WeightTrackerState has failed: " <> ls (show dbErr)
    Right (initialWeight, courierCalledToday, orders) -> do
       -- Create the initial state based on what's in the DB
      let initialState = WeightTrackerState
            { wtsCurrentDay = initialDay
            , wtsTotalWeightGrams = initialWeight
            , wtsCourierCalled = courierCalledToday
            , wtsOrders = orders
            }
      stateVar <- liftIO $ newTVarIO initialState

      let template_orders = T.unlines (map ((<>) "• `" . (`T.append` "`")) orders)
      let templateData = HM.fromList [ ("orders", template_orders), ("total_weight", tshow initialWeight)]
      let loadedTmpl | courierCalledToday = ".Init"
                     | otherwise = ".Pickup"
      msg <- fmap escapeMarkdownV2 $ render ($currentModule <> loadedTmpl) templateData
      void $ sendOrEditTelegramMessage mempty msg PICKUP Nothing Nothing Nothing

      -- Worker A: Periodically checks if we need to call the courier 
      let checkCourierPoller = runAppM $ callDostavistaCourier stateVar

      -- Worker B: Resets the 'courier called' flag at midnight
      let midnightResetter = runAppM $ resetCourierCalledFlag stateVar

      -- Worker C: Listens for NEW weighed orders and adds weight
      let weightAccumulator = runAppM $ runWeightAccumulator stateVar connInfo runAppM

      -- Worker D: Listens for status CHANGES and subtracts weight
      let statusChangeListener = runAppM $ runStatusChangeListener stateVar connInfo runAppM

      let workers = [checkCourierPoller, midnightResetter, weightAccumulator, statusChangeListener]

      -- 3. Run all three in parallel and wait for any of them to crash
      $(logTM) InfoS "Spawning all Daily Weight Tracker threads..."
      eResult <- liftIO $ try @SomeException $ waitAnyCatchCancel =<< mapM async workers

      -- If we are here, one of the threads has died.
      when (isLeft eResult) $ $(logTM) CriticalS $ "A Weight Tracker thread died with an exception: " <> ls (show (fromLeft undefined eResult))


-- | A worker that runs periodically to reset the 'courier called' flag after midnight.
resetCourierCalledFlag :: TVar WeightTrackerState -> AppM ()
resetCourierCalledFlag stateVar = forever $ do
  -- Sleep first, e.g., for 15 minutes
  liftIO $ threadDelay (15 * 60 * 1000000)

  -- Get current Moscow time and date
  let msk = TimeZone (3 * 60) False "MSK"
  now <- liftIO getZonedTime
  let today = localDay (utcToLocalTime msk (zonedTimeToUTC now))

  -- Check and potentially update the state atomically
  liftIO $ atomically $ do
    currentState <- readTVar stateVar
        
    -- If the day stored in our state is NOT today, it means a new day has begun.
    when (wtsCurrentDay currentState /= today) $ do
      -- Reset the flag, but CARRY OVER the weight and orders
      -- This handles items weighed after the last pickup of the previous day.
      writeTVar stateVar $ WeightTrackerState
        { wtsCurrentDay = today
        , wtsTotalWeightGrams = wtsTotalWeightGrams currentState
        , wtsCourierCalled = False -- <-- THE RESET
        , wtsOrders = wtsOrders currentState
        }

  $(logTM) DebugS "Ran daily state reset check."


runStatusChangeListener :: TVar WeightTrackerState -> PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runStatusChangeListener stateVar connInfo runAppM = do
  $(logTM) InfoS "Order Status Change Listener started."
  liftIO $ PG.withConnect connInfo $ \conn -> do
    void $ PG.execute_ conn "LISTEN order_weight_subtraction_events"    
    forever $ do
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
            
      case eitherDecode (BL.fromStrict payload) of
        Left err -> void $ runAppM $ $(logTM) ErrorS $ ls $ "Failed to parse payload (WeighedOrderEvent), error: " <> err -- Log error
        Right (WeighedOrderEvent{..}) -> do
          runAppM $ $(logTM) InfoS $ "Order " <> ls (show orderId) <> " changed status away from 'paid'. Subtracting weight." 
          liftIO $ atomically $ do
            -- Atomically subtract the weight and remove the order from the list
            modifyTVar' stateVar $ \s ->
              s { wtsTotalWeightGrams =
                    wtsTotalWeightGrams s - weightGrams
                , wtsOrders = 
                    delete orderId (wtsOrders s)
                }
          state <- liftIO $ atomically $ readTVar stateVar    
          void $ runAppM $ do
            let templateData = 
                  HM.fromList 
                  [ ("order_id", orderId)
                  , ("weight", tshow weightGrams)
                  , ("total_weight", tshow (wtsTotalWeightGrams state + weightGrams))
                  ]
            msg <- fmap escapeMarkdownV2 $ render ($currentModule <> ".DropWeight") templateData
            void $ sendOrEditTelegramMessage mempty msg PICKUP Nothing Nothing Nothing

runWeightAccumulator :: TVar WeightTrackerState -> PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runWeightAccumulator stateVar connInfo runAppM = 
  liftIO $ PG.withConnect connInfo $ \conn -> do
    runAppM $ $(logTM) DebugS "order weighed listener connected ..."
    let cmd = fromString $ T.unpack $ TE.decodeUtf8 $ $(embedFile "sql/order_weighed_events")
    void $ PG.execute_ conn cmd
    forever $ do
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      let ePayload = eitherDecode @WeighedOrderEvent $ BL.fromStrict payload
      -- 3. Fork a thread to process the event
      void $ async $ void $ runAppM $ runJobWithCleanup (processWeightEvent stateVar ePayload)


-- This is an IO action that returns a (Bool, Bool).
isBusinessDay :: IO (Bool, Bool)
isBusinessDay = do
  -- 1. Get the current time in UTC
  nowUTC <- getCurrentTime

  -- 2. Get the user's current timezone
  timezone <- getCurrentTimeZone

  -- 3. Convert the UTC time to the local time
  let localTime = utcToLocalTime timezone nowUTC
  
  -- 4. Extract the DayOfWeek (Monday, Tuesday, etc.) from the local time
  let today = dayOfWeek (localDay localTime)
  
  -- 5. Define which days are considered business days
  let businessDays = [Monday, Tuesday, Wednesday, Thursday, Friday]
  
  -- 6. Check if today is in the list and return the result
  return (today `elem` businessDays, today == Monday)


-- The logic for processing a single event
processWeightEvent :: TVar WeightTrackerState -> Either String WeighedOrderEvent -> AppM ()
processWeightEvent _ (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (WeighedOrderEvent), error: " <> err
processWeightEvent stateVar (Right WeighedOrderEvent{..}) = do
  cfg <- ask
  let weightThreshold = _courierWeightThreshold cfg -- e.g., 5000 (grams) from config

  -- Get current Moscow hour
  let msk = TimeZone (3 * 60) False "MSK"
  now <- liftIO getZonedTime
  let mskTime = utcToLocalTime msk (zonedTimeToUTC now)
  let (TimeOfDay currentHour _ _) = localTimeOfDay mskTime

  -- Get current state and update it atomically
  tm <- currentTime
  let today = utctDay tm
  updatedState <- liftIO $ atomically $ do
     currentState <- readTVar stateVar            
     -- Check if it's a new day. If so, reset the counter.
     let (oldWeight, alreadyCalled)
            | wtsCurrentDay currentState == today
              = (wtsTotalWeightGrams currentState
               , wtsCourierCalled currentState)
              -- Reset for the new day, but only flag is_courier_called 
              -- on account of there may have been order collected since the last pick-up  
            | otherwise = (wtsTotalWeightGrams currentState, False)
     let newWeight = oldWeight + weightGrams
                
     -- The new state object
     let newOrders = orderId : wtsOrders currentState
     let newState = WeightTrackerState today newWeight alreadyCalled newOrders
     fmap (const newState) $ writeTVar stateVar newState

  -- Check the threshold
  $(logTM) InfoS $ "Order weighed. Current daily total: " <> ls (show $ wtsTotalWeightGrams updatedState) <> "g"

  -- add message to order channel
  let templateData = 
        HM.fromList 
        [ ("order_id", orderId)
        , ("weight", tshow weightGrams)
        , ("total_weight", tshow $ wtsTotalWeightGrams updatedState - weightGrams)
        ]

  pool <- fmap _appDBPool ask
  void $ updateOrderStatus orderId AddedToPickupQueue pool

  msg <- fmap escapeMarkdownV2 $ render ($currentModule <> ".AddWeight") templateData
  void $ sendOrEditTelegramMessage mempty msg PICKUP Nothing Nothing Nothing


callDostavistaCourier :: TVar WeightTrackerState -> AppM ()
callDostavistaCourier stateVar = forever $ do 
  $(logTM) InfoS "starting measuring weight for courier call..."
  WeightTrackerState {..} <- readTVarIO stateVar
  cfg <- ask
  let weightThreshold = _courierWeightThreshold cfg
   -- Get current Moscow hour
  let msk = TimeZone (3 * 60) False "MSK"
  now <- liftIO getZonedTime
  let mskTime = utcToLocalTime msk (zonedTimeToUTC now)
  let (TimeOfDay currentHour _ _) = localTimeOfDay mskTime
  let weightExceeded = wtsTotalWeightGrams >= weightThreshold
  let courierNotCalled = not wtsCourierCalled

  (dayOfWeek, isMonday) <- liftIO isBusinessDay
  let cutoffHour | isMonday = 3 + (courierCallCutoffHour . _dostavistaConfig) cfg
                 | otherwise = (courierCallCutoffHour . _dostavistaConfig) cfg

  let isWithinSchedulingWindow =
       let hourBeforeCutoff = cutoffHour - 1
       in currentHour >= hourBeforeCutoff &&
          currentHour < cutoffHour

  when (not dayOfWeek) $ $(logTM) InfoS $ "Weekend. skip the courier call"

  when(dayOfWeek &&
       weightExceeded &&
       courierNotCalled && 
       isWithinSchedulingWindow) $ do
    $(logTM) InfoS $ "Weight threshold exceeded within time window. Calling courier..."    
    -- Call the Dostavista Service
    pool <- fmap _appDBPool ask
    eDbRes <- fetchDostavistaPackages wtsOrders pool
    when(isLeft eDbRes) $ do
      $(logTM) ErrorS $ ls $ "ddb failure " <> tshow eDbRes
      throwM err500
    let Right dostavistaPackages = eDbRes
    eDostavistaResult <- scheduleDostavistaPickup dostavistaPackages wtsTotalWeightGrams
    case eDostavistaResult of
      Left apiErr -> $(logTM) ErrorS $ "Dostavista API call failed: " <> ls (show apiErr)
      Right ord@DostavistaOrderResponse {..} ->
        if isSuccessful then do
          let Just Order {..} = order    
          -- Dostavista call was successful. Now, update both the
          -- in-memory TVar AND the database.
          $(logTM) InfoS "Dostavista call successful. Persisting state."
          $(logTM) DebugS $ "Dostavista order: " <> ls (show ord)          
          -- Persist the "called" state to the database
          -- Get the list of orders to include in this batch from the state
          let ordersInBatch = wtsOrders

          tm <- currentTime
          let today = utctDay tm
          let courierPickupData = 
               CourierPickupData 
                { cpdDay                   = today 
                , cpdProvider              = DOSTAVISTA
                , cpdOrders                = ordersInBatch
                , cpdDostavistaOrderId     = orderId
                , cpdDostavistaOrderStatus = encodeToText status
                , cpdCost                  = 
                  fromMaybe (error "cannot extract paymentAmount") $
                    textMoneyToDouble paymentAmount
                , cpdOrderStatus           = encodeToText ScheduledForPickup   
                }
          let debugMsg = decodeUtf8 $ BL.toStrict $ encodePretty courierPickupData
          $(logTM) InfoS $ "CourierPickupData: " <> ls debugMsg
          eDbResult <- recordAndLinkPickup courierPickupData pool
          for_ eDbResult $ const $ do
            -- Update the in-memory flag immediately
            modifyTVarIO stateVar $
              \s -> s { wtsTotalWeightGrams = 0
                      -- Still true for today, will be reset tomorrow
                      , wtsCourierCalled = True
                      -- Clear the list for the next batch
                      , wtsOrders = [] }

            -- delegate the tracking to Dostavista worker
            appStateVar <- get
            appState <- readTVarIO appStateVar
            start <- currentTime
            liftIO $ atomically $ writeTChan (_dostavistaChan appState) $ DostavistaJob orderId status start

            -- Send a detailed notification to the admin channel
            let templateData = 
                  HM.fromList
                  [ ("count", tshow (length ordersInBatch))
                  , ("weight", tshow wtsTotalWeightGrams)
                  , ("order_list",  T.unlines (map ((<>) "• `" . (`T.append` "`")) ordersInBatch))
                  ]
            msg <- fmap escapeMarkdownV2 $ render $currentModule templateData
            void $ sendOrEditTelegramMessage mempty msg PICKUP Nothing Nothing Nothing
        else $(logTM) ErrorS "Dostativsta call has ended up in failure"
  liftIO $ threadDelay (10 * 60 * 1000000) -- sleep 10 minutes before re-evaluating
