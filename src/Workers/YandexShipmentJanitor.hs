{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}


module Workers.YandexShipmentJanitor (runYandexShipmentJanitor) where


import Katip (logTM, Severity(..), ls)
import Data.Time
import Text.Printf (printf)
import qualified Data.Text as T
import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import  Data.List (find, maximumBy)
import  Data.Ord (comparing)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay, Day)
import Data.Time.Calendar.WeekDate (dayOfWeek)
import Data.Time.Calendar (addDays, toGregorian)
import Data.Time.LocalTime (localTimeOfDay, TimeOfDay(..), utcToLocalTime, zonedTimeToUTC, TimeZone(..))


import Text (tshow)
import App (AppM, _appDBPool, ChatKey (PICKUP), _yandexWarehouseId, readTVarIO, _sdekConfig, _yandexConfig)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Yandex.Shipment
import qualified Infrastructure.Services.Yandex.Types as Ty (PickupOptionsRespItem (..))
import qualified Infrastructure.Services.Yandex.Types as Ya.CS (CreateShipmentReq (..))
import Infrastructure.Services.Yandex.Error (getError, getHttpException)
import Infrastructure.Database (fetchEmptyPickupForTomorrow, eraseEmptyTomorrowPickup, savePickupDetails)
import qualified Infrastructure.Services.Yandex as Ya
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import qualified Infrastructure.Services.Sdek.Types.Config as SdekCfg
import qualified Infrastructure.Services.Yandex.Config as YaCfg


-- | Background loop that manages daily Yandex logistics tasks
runYandexShipmentJanitor :: AppM ()
runYandexShipmentJanitor = do

  sdekCfg <- fmap _sdekConfig ask


  let msk = TimeZone (3 * 60) False "MSK"
  now <- liftIO getZonedTime
  let mskLocalTime = utcToLocalTime msk (zonedTimeToUTC now)
  let today        = localDay mskLocalTime
  let day          = dayOfWeek today
  let days         = Sunday : [Monday .. Thursday]
  let isWeekday    = day `elem` days

  let tomorrow = addDays 1 today
  let (_, tMonth, tDay) = toGregorian tomorrow -- returns (Year, Month, Day)
  let isTomorrowHoliday = 
         flip any (SdekCfg.holidays sdekCfg) $ \h -> 
           SdekCfg.month h == tMonth && 
           SdekCfg.day h == tDay

  yaCfg <- fmap _yandexConfig ask

  -- 1. Get current UTC time
  now <- liftIO getCurrentTime

  -- 2. Moscow is UTC+3 (Note: Russia does not observe DST)
  let moscowZone   = hoursToTimeZone 3
      localNow     = utcToLocalTime moscowZone now
      timeOfDayNow = localTimeOfDay localNow
      hour         = todHour timeOfDayNow

  when (isWeekday && not isTomorrowHoliday) $ do
    --  Trigger logic based on Moscow Local Time
    when(YaCfg.shipmentCreation yaCfg == hour) $ do
      $(logTM) InfoS
        "Yandex shipment Janitor: \
        \ Starting morning createShipment job..."
      createShipment
        
    when(YaCfg.shipmentCancellation yaCfg == hour) $ do
      $(logTM) InfoS 
        "Yandex shipment Janitor: \
        \ Starting evening clearShipment job..."
      clearShipment


-- =============================================================================
-- sub function
-- =============================================================================

createShipment :: AppM ()
createShipment = do
  today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  let tomorrow = addDays 1 today
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- fetchEmptyPickupForTomorrow tomorrow pool
  case eDbRes of
    Left err -> 
      $(logTM) ErrorS $ ls $ 
        "fetchEmptyPickupForTomorrow \
        \ failed: " <> tshow err
    -- skip
    Right (Just _) -> pure ()
    Right (Nothing) -> do
      stateVar <- get
      maybeWarehouseId <- fmap _yandexWarehouseId $ readTVarIO stateVar
      let warehouseId = 
           Ya.platformStationId $ 
            fromMaybe 
              (error "station id not set") 
              maybeWarehouseId
    
      ePickupOp <- Ya.getPickupOptions $ Ya.PickupOptionsReq warehouseId

      case ePickupOp of
        Left err -> do
          $(logTM) ErrorS $ ls $ "getPickupOptions failed: " <> tshow err
          let maybeHttpExcep = getHttpException err
          for_ maybeHttpExcep $ \excep -> do
            let errMsg = escapeMarkdownV2 $ "‼️ YANDEX: " <> getError excep
            void $ sendOrEditTelegramMessage mempty errMsg PICKUP Nothing Nothing Nothing
        Right Ya.PickupOptionsResp {Ya.pickupOptions} -> do
          let interval = selectPickupWindow pickupOptions tomorrow
          let shipmentReq =
                Ya.CS.CreateShipmentReq
                { Ya.CS.parameters              = makePickupParams 
                , Ya.CS.pickupLocalDate         = tshow tomorrow
                , Ya.CS.pickupLocalTimeInterval = interval
                , Ya.CS.stationId               = warehouseId
                }
          ePickupId <- Ya.createShipment shipmentReq
          case ePickupId of
            Left err -> do
              $(logTM) ErrorS $ ls $ "createPickup failed: " <> tshow err
              let maybeHttpExcep = getHttpException err
              for_ maybeHttpExcep $ \excep -> do
                let errMsg = escapeMarkdownV2 $ "‼️ YANDEX: " <> getError excep
                void $ sendOrEditTelegramMessage mempty errMsg PICKUP Nothing Nothing Nothing
            Right Ya.CreateShipmentResp {Ya.pickupId} -> do
              void $ savePickupDetails pickupId tomorrow pool
              let msg = escapeMarkdownV2 $ 
                          "YANDEX: pickup is scheduled for " <> 
                          tshow tomorrow <> ", from " <> from  interval <> ", to " <> to interval 
              void $ sendOrEditTelegramMessage mempty msg PICKUP Nothing Nothing Nothing


-- | Implementation of the aggregate calculation for Yandex Courier Pickup
makePickupParams :: PickupParameters
makePickupParams  = 
    PickupParameters
    { -- FORMATTING FIX: Use "%.2f" to match the Yandex Regex [0-9]{1,2}
      -- This converts 0.0636 -> "0.06"
      volumeM3     = "0.50"
    , weightG      = 25000
    , requirements = Nothing
    }


-- | Selects the best available pickup window for a specific date
-- | Targeted preference: 12:00 - 18:00
selectPickupWindow :: [Ty.PickupOptionsRespItem] -> Day -> LocalTimeInterval
selectPickupWindow options targetDay =
    let 
        -- 1. Format date to "YYYY-MM-DD" to match Yandex string
        targetDateStr = targetDay
        
        -- 2. Find the entry for the requested day
        mDayItem = find (\item -> Ty.pickupLocalDate item == targetDateStr) options
        mInterval = mDayItem <&> \dayItem ->
          -- 3. Rank intervals by how many 'preferred hours' they contain
          -- (Preferred: 12, 13, 14, 15, 16, 17, 18)
          maximumBy (comparing (scoreInterval 12 18)) (Ty.pickupLocalTimeIntervals dayItem)
    in fromMaybe defLocalTimeInterval mInterval


-- | Scores an interval by checking if our target start/end hours fit inside.
-- | A slot [09:00-20:00] scores higher for a [12:00-18:00] request than [06:00-08:00].
scoreInterval :: Int -> Int -> LocalTimeInterval -> Int
scoreInterval prefStart prefEnd interval =
    let 
        -- Parse hours from "HH:MM"
        getHour t = read (T.unpack $ T.take 2 t) :: Int
        startH = getHour (from interval)
        endH   = getHour (to interval)
        
        -- Logic: check overlap
        -- If the interval contains the start of our preferred window, give high score
        score = if startH <= prefStart && endH >= prefEnd then 10 
                else if startH <= prefStart && endH > prefStart then 5
                else 0
    in score


clearShipment :: AppM ()
clearShipment = do
  today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  let tomorrow = addDays 1 today
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- fetchEmptyPickupForTomorrow tomorrow pool
  case eDbRes of
    Left err -> 
      $(logTM) ErrorS $ ls $ 
        "fetchEmptyPickupForTomorrow \
        \ failed: " <> tshow err
    Right isEmpty ->
      for_ isEmpty $ \pickupId -> do
       ePickupResp <- Ya.cancelShipment $ Ya.CancelPickupReq pickupId
       case ePickupResp of
         Left err -> do
           $(logTM) ErrorS $ ls $ 
            "cancelShipment failed: " <> tshow err
           let maybeHttpExcep = getHttpException err
           for_ maybeHttpExcep $ \excep -> do
              let errMsg =
                   escapeMarkdownV2 $ 
                     "‼️ YANDEX CANCEL FAILED: " <>
                     getError excep
              void $ sendOrEditTelegramMessage mempty errMsg PICKUP Nothing Nothing Nothing
         Right _ -> void $ eraseEmptyTomorrowPickup pickupId pool