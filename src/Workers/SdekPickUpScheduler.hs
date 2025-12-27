{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Workers.SdekPickUpScheduler (runSdekPickUpScheduler) where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Katip
import Data.Time -- for time-of-day checking
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar.WeekDate (dayOfWeek)
import Data.Time.LocalTime (localTimeOfDay, TimeOfDay(..), utcToLocalTime, zonedTimeToUTC, TimeZone(..))
import Control.Monad.Reader.Class (ask)

import App (AppM, _sdekConfig)
import Domain.Services.Shipping (prepareAndSchedulePickup)
import Infrastructure.Services.Sdek.Types.Config


-- | The main loop for the scheduled SDEK pickup job.
runSdekPickUpScheduler :: AppM ()
runSdekPickUpScheduler = do
    $(logTM) InfoS "SDEK Pickup Scheduler thread started."
    forever $ do
        -- 1. Get the current time in the correct timezone (e.g., Moscow)
        let msk = TimeZone (3 * 60) False "MSK"
        now <- liftIO $ getZonedTime
        let mskTime = utcToLocalTime msk (zonedTimeToUTC now)
        let (TimeOfDay hour _ _) = localTimeOfDay mskTime
        let day = dayOfWeek (localDay mskTime)
        
        -- 2. Check if it's the right time to run
        --    e.g., between 5 PM and 6 PM on a weekday.
        let isWeekday = day `elem` [Monday .. Friday]
        cfg <- fmap _sdekConfig ask
        let pickupWindowStartText = from (pickupWindow cfg)
        let parseHourResult = parseHour pickupWindowStartText
        case parseHourResult of
          Nothing -> 
            $(logTM) ErrorS $ ls $ "Invalid pickup window start time format in config: " <> pickupWindowStartText
          Just pickupWindowStart -> do 
            let isPickupWindow = 
                  hour == pickupWindowStart -- Run at 5 PM (17:00)
            if isWeekday && 
               isPickupWindow
            then do
                $(logTM) InfoS "It's pickup time. Running SDEK courier scheduling..."
                res <- prepareAndSchedulePickup
                $(logTM) InfoS $ ls $ "SDEK courier scheduling finished for this window with result " <> show res <> "."
                -- Sleep for just over an hour to ensure we don't run twice in the same window
                liftIO $ threadDelay (3700 * 1000000) -- Sleep ~61 minutes
            else
                -- It's not time yet. Sleep for a shorter interval and check again.
                -- Check every 15 minutes.
                liftIO $ threadDelay (15 * 60 * 1000000)