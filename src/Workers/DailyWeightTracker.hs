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
import Servant.Server (ServerError)
import qualified Data.Text as T
import Data.Foldable (for_)
import Data.Either (isLeft, fromLeft)
import Control.Exception (SomeException, try)
import Data.Maybe (fromMaybe)
import Data.List (delete)
import Data.FileEmbed (embedFile)
import qualified Data.Text.Encoding as TE
import Control.Concurrent.Async (async, waitAnyCatchCancel)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar, atomically, modifyTVar', TChan, writeTChan)
import Data.Time (Day, UTCTime, getCurrentTime, utctDay)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import Data.Time.LocalTime (TimeZone (..), getZonedTime, zonedTimeToUTC, utcToLocalTime, localTimeOfDay, TimeOfDay (..), localDay)

import App
import API.Types (OrderStatus (ScheduledForPickup))
import Text (camelToSnake, tshow, textMoneyToDouble, encodeToText)
import Concurrency (runJobWithCleanup)
import TH.Location (currentModule)
import Infrastructure.Services.Dostavista.Types
import Infrastructure.Services.Dostavista (scheduleDostavistaPickup)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import  Infrastructure.Services.Dostavista.Types.Config (courierCallCutoffHour)
import Infrastructure.Database (recordAndLinkPickup, fetchWeightTrackerStateInfo, CourierService (DOSTAVISTA), CourierPickupData (..))


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
      void $ sendOrEditTelegramMessage mempty msg ORDER Nothing Nothing Nothing

      -- Worker A: Resets the 'courier called' flag at midnight
      let midnightResetter = runAppM (resetCourierCalledFlag stateVar)

      -- Worker B: Listens for NEW weighed orders and adds weight
      let weightAccumulator = runAppM $ runWeightAccumulator stateVar connInfo runAppM

      -- Worker C: Listens for status CHANGES and subtracts weight
      let statusChangeListener = runAppM $ runStatusChangeListener stateVar connInfo runAppM

      let workers = [midnightResetter, weightAccumulator, statusChangeListener]

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
            void $ sendOrEditTelegramMessage mempty msg ORDER Nothing Nothing Nothing

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
  msg <- fmap escapeMarkdownV2 $ render ($currentModule <> ".AddWeight") templateData
  void $ sendOrEditTelegramMessage mempty msg ORDER Nothing Nothing Nothing


  let totalWeight = wtsTotalWeightGrams updatedState
  let weightExceeded = totalWeight >= weightThreshold
  let courierNotCalled = not (wtsCourierCalled updatedState)
  let isWithinTimeWindow = currentHour < (courierCallCutoffHour . _dostavistaConfig) cfg -- e.g., Is current hour < 16?

  if weightExceeded && 
     courierNotCalled && 
     isWithinTimeWindow
  then do
    $(logTM) InfoS $ "Weight threshold exceeded within time window. Calling courier..."            
    -- Call the Dostavista Service
    eDostavistaResult <- scheduleDostavistaPickup totalWeight
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
          pool <- fmap _appDBPool ask
          -- Get the list of orders to include in this batch from the state
          let ordersInBatch = wtsOrders updatedState

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
                  , ("weight", tshow totalWeight)
                  , ("order_list",  T.unlines (map ((<>) "• `" . (`T.append` "`")) ordersInBatch))
                  ]
            msg <- fmap escapeMarkdownV2 $ render $currentModule templateData
            void $ sendOrEditTelegramMessage mempty msg ORDER Nothing Nothing Nothing
        else $(logTM) ErrorS "Dostativsta call has ended up in failure"        

  else when (weightExceeded && 
             courierNotCalled && 
             not isWithinTimeWindow) $
         -- The weight is high enough, but it's too late in the day.
         $(logTM) WarningS "Weight threshold met, but it's too late to call a courier today. Manual action may be required."
         -- Optionally, send an alert to the admin here.
