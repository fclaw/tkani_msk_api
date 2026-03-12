{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Workers.CourierPickUpScheduler (runCourierPickUpScheduler) where


import Katip
import Data.Time -- for time-of-day checking
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar.WeekDate (dayOfWeek)
import Data.Time.Calendar (addDays, toGregorian)
import Data.Time.LocalTime (localTimeOfDay, TimeOfDay(..), utcToLocalTime, zonedTimeToUTC, TimeZone(..))
import Control.Monad.Reader.Class (ask)
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar, atomically)
import Data.Maybe (fromMaybe)
import Control.Monad (when, void)

import App (AppM, _sdekConfig)
import qualified Domain.Services.Shipping.Sdek as Sdek (prepareAndSchedulePickup)
import qualified Domain.Services.Shipping.Yandex as Yandex (prepareAndSchedulePickup)
import Infrastructure.Services.Sdek.Types.Config
import qualified Infrastructure.Services.Sdek.Types.Config as Cfg


-- | The main loop for the scheduled courier pickup job.
-- | A state for the scheduler to remember the date of its last successful run.
type LastRunDay = TVar (Maybe Day)

-- | The main scheduler function. It takes the TVar as an argument.
runCourierPickUpScheduler :: TVar (Maybe Day) -> AppM ()
runCourierPickUpScheduler lastRunVar = do
  $(logTM) InfoS "Courier Pickup Scheduler thread started."
  -- A. Get current time info
  let msk = TimeZone (3 * 60) False "MSK"
  now <- liftIO getZonedTime
  let mskLocalTime = utcToLocalTime msk (zonedTimeToUTC now)
  let (TimeOfDay hour _ _) = localTimeOfDay mskLocalTime
  let today = localDay mskLocalTime
  let day = dayOfWeek today

  -- B. Get config for the pickup window
  sdekCfg <- fmap _sdekConfig ask

  -- C. Check if we need to run the job
  let isRightTime = hour == consolidationTime sdekCfg
  let days = Sunday : [Monday .. Thursday]
  let isWeekday = day `elem` days
 
  let tomorrow = addDays 1 today
  let (_, tMonth, tDay) = toGregorian tomorrow -- returns (Year, Month, Day)
  let isTomorrowHoliday = any (\h -> Cfg.month h == tMonth && Cfg.day h == tDay) (holidays sdekCfg)

  -- D. Atomically check the lock
  shouldRun <- liftIO $ atomically $ do
    lastRun <- readTVar lastRunVar
    -- Condition: We should run if it's the right time and we haven't already run today.
    if isRightTime && 
       isWeekday &&
       not isTomorrowHoliday &&
       lastRun /= Just today
    then do
      -- If we decide to run, we immediately "take the lock"
      -- by writing today's date into the TVar.
      writeTVar lastRunVar (Just today)
      return True
    else return False
        
  -- E. Execute the job if the check passed
  when shouldRun $ do
    $(logTM) InfoS "Pickup window is open. Running services courier scheduling..."
    isSdekFinished   <- Sdek.prepareAndSchedulePickup
    isYandexFinished <- Yandex.prepareAndSchedulePickup
    when (isSdekFinished) $
      $(logTM) InfoS "Sdek courier scheduling finished."
    when (isYandexFinished) $
      $(logTM) InfoS "Yandex courier scheduling finished."
    $(logTM) InfoS "courier routine finished."

  -- F. If it is a new day (after midnight), reset the lock.
  liftIO $ atomically $ do
    lastRun <- readTVar lastRunVar
    when (lastRun /= Just today) $
      writeTVar lastRunVar Nothing