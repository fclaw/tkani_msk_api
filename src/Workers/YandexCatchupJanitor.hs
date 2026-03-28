{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Workers.YandexCatchupJanitor (runYandexCatchupJanitor) where

import Katip
import Data.Foldable (for_)
import Control.Monad (when)
import Data.Time (getZonedTime, localDay)
import Control.Monad.Reader.Class (ask)
import Data.Time.Calendar.WeekDate (dayOfWeek)
import Data.Time.Calendar (addDays, toGregorian, DayOfWeek(Sunday))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (TVar, readTVar, writeTVar, atomically)
import Data.Time.LocalTime (localTimeOfDay, TimeOfDay(..), utcToLocalTime, zonedTimeToUTC, TimeZone(..))


import Text (tshow)
import App (AppM, _appDBPool)
import Workers.CourierPickUpScheduler (LastRunDay)
import Infrastructure.Database (fetchCatchupYandexOrders)
import Workers.YandexPrepaidOrderRegistrar (registerOrder)


runYandexCatchupJanitor :: LastRunDay -> AppM ()
runYandexCatchupJanitor lastRunVar = do
  $(logTM) InfoS "Running Yandex Catch-up Janitor..."

  let msk = TimeZone (3 * 60) False "MSK"
  now <- liftIO getZonedTime
  let mskLocalTime = utcToLocalTime msk (zonedTimeToUTC now)
  let (TimeOfDay hour _ _) = localTimeOfDay mskLocalTime
  let today = localDay mskLocalTime
  let day = dayOfWeek today

  -- Atomically check the lock
  shouldRun <- liftIO $ atomically $ do
    lastRun <- readTVar lastRunVar
    -- Condition: We should run if it's the right time and we haven't already run today.
    if day == Sunday && hour == 12
    then do
      -- If we decide to run, we immediately "take the lock"
      -- by writing today's date into the TVar.
      writeTVar lastRunVar (Just today)
      return True
    else return False

  when shouldRun $ do
    $(logTM) InfoS "Yandex Catch-up Janitor is starting its work..."
    cfg <- ask
    let pool = _appDBPool cfg
    eOrders <- fetchCatchupYandexOrders pool
    case eOrders of
      Left err -> 
        $(logTM) ErrorS $ ls $ 
        "Failed to fetch catch-up orders: " <> err
      Right orders -> do
        $(logTM) InfoS $ ls $ 
          "Found " <>
          tshow (length orders) <>
          " catch-up orders to process."
        for_ orders $ \ (orderId, amount, days) -> do
          let intAmount = fromIntegral amount
          let intDays = fromIntegral days
          registerOrder orderId intAmount intDays

    -- After processing, we can release the lock for the next week.
  liftIO $ atomically $ do
    lastRun <- readTVar lastRunVar
    when (lastRun /= Just today) $
      writeTVar lastRunVar Nothing