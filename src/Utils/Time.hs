module Utils.Time (utcToMoscowZonedTime, utcToMoscowLocalTime) where

-- Make sure you have these imports
import Data.Time (UTCTime, TimeZone(..), ZonedTime(..), utcToZonedTime, utcToLocalTime, LocalTime)

-- | Defines the Moscow TimeZone (MSK), which is UTC+3.
--   Moscow no longer observes Daylight Saving Time, so the 'timeZoneSummerOnly' is False.
moscowTimeZone :: TimeZone
moscowTimeZone = TimeZone
    { timeZoneMinutes = 180  -- 3 hours * 60 minutes
    , timeZoneSummerOnly = False
    , timeZoneName = "MSK"
    }

-- | Converts a UTCTime to a ZonedTime in the Moscow timezone.
--   A ZonedTime contains both the local time AND the timezone info.
utcToMoscowZonedTime :: UTCTime -> ZonedTime
utcToMoscowZonedTime = utcToZonedTime moscowTimeZone

-- | Converts a UTCTime to a LocalTime in the Moscow timezone.
--   A LocalTime is just the date and time-of-day, without the timezone info.
--   This is often what you need for simple display purposes.
utcToMoscowLocalTime :: UTCTime -> LocalTime
utcToMoscowLocalTime = utcToLocalTime moscowTimeZone