{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Domain.Services.Shipping.Yandex (prepareAndSchedulePickup) where


import Data.Functor ((<&>))
import Control.Monad (void)
import qualified Data.Text as T
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Katip (logTM, Severity(..), ls)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import  Data.List (find, maximumBy)
import  Data.Ord (comparing)
import Data.Time.Calendar (addDays, showGregorian)
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay, Day)

import Text (tshow)
import App (AppM, _yandexConfig, _appDBPool, ChatKey(PICKUP), readTVarIO, _yandexWarehouseId)
import API.Types (OrderStatus (ScheduledForPickup))
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Yandex.Shipment
import Infrastructure.Services.Yandex (generateManifest, createShipment, getPickupOptions)
import qualified Infrastructure.Services.Yandex.Types as Ty (PickupOptionsRespItem (..))
import Infrastructure.Services.Yandex.Types (PickupOptionsResp (..), PickupOptionsReq (..), ManifestReq (..), CreateShipmentResp (..), CreateShipmentReq (..), PlatformStationId (..))
import Infrastructure.Services.Yandex.Config (pickupParcels, pickupWeight, pickupWindow, fromHour, toHour)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, sendDocument)
import Infrastructure.Database (fetchOrdersForYandexCourierPickup, savePickupDetails, OrdersForYandexCourierPickupItem (..))


prepareAndSchedulePickup :: AppM Bool
prepareAndSchedulePickup = do 
  $(logTM) InfoS "Checking for paid orders to schedule for YANDEX pickup..."
  -- Get the current date to pass to the query for the idempotency check
  today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  -- 1. Atomically find and update the orders.
  --    The query now has built-in guards.
  cfg <- ask
  let pool = _appDBPool cfg
  let yaConfig = _yandexConfig cfg
  let countThreshold = pickupParcels yaConfig
  let weightThreshold = pickupWeight yaConfig
  eOrdersToSchedule <- fetchOrdersForYandexCourierPickup ScheduledForPickup pool
  case eOrdersToSchedule of
    Left dbErr -> 
      fmap (const False) $ 
        $(logTM) ErrorS $ ls $ 
          "DB error while fetching \
          \ paid orders: " <> tshow dbErr
    Right Nothing -> fmap (const False) $ $(logTM) InfoS $ "No new paid orders to schedule."
    Right (Just orders) ->
      if null orders then
        fmap (const False) $ $(logTM) InfoS $ "No new paid orders to schedule."
      else if not (checkRequirements orders countThreshold weightThreshold) then do
        -- --- This is the refined logging message ---
        let totalWeight =  sum $ orders <&> \OrdersForYandexCourierPickupItem {..} -> oycpiWeight
        let totalParcelsCount = length orders
        let notMetMsg = 
              "Requirements not met to call courier. " <>
              "Current state: " <>
              "Parcels Count = " <> 
              tshow totalParcelsCount <> 
              " (Threshold = " <> 
              tshow countThreshold <> "), " <>
              "Total Weight = " <>
              tshow totalWeight <> 
              " g (Threshold = " <> 
              tshow weightThreshold <> " g). " <>
              "Waiting for more orders or a heavier batch."
        $(logTM) InfoS $ ls notMetMsg
        fmap (const False) $ sendOrEditTelegramMessage mempty notMetMsg PICKUP Nothing Nothing Nothing
      else do
        -- We have enough orders to schedule a pickup
        $(logTM) InfoS "Scheduling YANDEX courier pickup for orders..."          
        -- ... (the rest of your logic to call the SDEK API) ...
        $(logTM) InfoS $ ls $ "Found " <> tshow (length orders) <> " orders. Scheduling courier..."
        let manifestReq = ManifestReq $ map oycpiRequestId orders
        eManifest <- generateManifest manifestReq
        case eManifest of
          Left err -> do
            $(logTM) ErrorS $ ls $ "generateManifest failed: " <> tshow err
            let error = escapeMarkdownV2 $ "‼️ Error in calling generateManifest: " <> tshow err
            fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
          Right pdfBytes -> do
            stateVar <- get
            maybeWarehouseId <- fmap _yandexWarehouseId $ readTVarIO stateVar
            let warehouseId = 
                 platformStationId $ 
                  fromMaybe 
                    (error "station id not set") 
                    maybeWarehouseId
    
            ePickupOp <- getPickupOptions $ PickupOptionsReq warehouseId

            case ePickupOp of
              Left err -> do
                $(logTM) ErrorS $ ls $ "getPickupOptions failed: " <> tshow err
                let error = escapeMarkdownV2 $ "‼️ Error in calling getPickupOptions: " <> tshow err
                fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
              Right PickupOptionsResp {pickupOptions} -> do
                let mInterval = selectPickupWindow pickupOptions (addDays 1 today)
                case mInterval of 
                  Nothing -> do
                    $(logTM) WarningS "pickup interval has not been found"
                    let warning = escapeMarkdownV2 $ "‼️ pickup interval has not been found"
                    fmap (const False) $ sendOrEditTelegramMessage mempty warning PICKUP Nothing Nothing Nothing
                  Just interval -> do
                    let shipmentReq =
                         CreateShipmentReq
                         { parameters              = makePickupParams orders
                         , pickupLocalDate         = tshow (addDays 1 today)
                         , pickupLocalTimeInterval = interval
                         , stationId               = warehouseId
                         }
                    ePickupId <- createShipment shipmentReq
                    case ePickupId of
                      Left err -> do
                        $(logTM) ErrorS $ ls $ "createPickup failed: " <> tshow err
                        let error = escapeMarkdownV2 $ "‼️ Error in calling createPickup: " <> tshow err
                        fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
                      Right CreateShipmentResp {pickupId} -> do
                        eBdRes <- savePickupDetails pickupId (addDays 1 today) pool
                        case eBdRes of 
                          Right _ -> do
                            -- send manifest to the PICKUP channel
                            let caption = escapeMarkdownV2 $ 
                                            "the YANDEX courier call has \
                                            \ been registered for " <>
                                            (tshow (addDays 1 today)) <>
                                            ", pickup window: " <> 
                                            from interval <> 
                                            " - " <> 
                                            to interval
                            let filename = "pickup-manifest-" <> tshow today <> ".pdf"
                            -- 2. Call the new service function
                            eTelResp <- sendDocument PICKUP caption filename pdfBytes "application/pdf"
                            liftIO $ print eTelResp
                            fmap (const True) $ $(logTM) InfoS $ "Successfully sent YANDEX pickup manifest for " <> ls (tshow today)
                          Left err -> do
                            $(logTM) ErrorS $ ls $ "savePickupDetails failed: " <> tshow err
                            let error = escapeMarkdownV2 $ "‼️ Error in calling savePickupDetails: " <> tshow err
                            fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing



-- | Checks if the given list of orders meets the requirements to call a courier.
--
-- Conditions:
--   1. Total parcels count exceeds 'countThreshold'.
--   2. OR total parcels count is less than or equal to 'countThreshold'
--      AND the total weight exceeds 'weightThreshold'.
--
-- This implies that if you have many parcels, call a courier regardless of weight.
-- But if you have few parcels, only call a courier if they are collectively heavy enough.
--
checkRequirements ::  [OrdersForYandexCourierPickupItem] -> Int -> Int -> Bool
checkRequirements orders countThreshold weightThreshold =
  let totalWeight = fromIntegral $ sum $ orders <&> \OrdersForYandexCourierPickupItem {..} -> oycpiWeight
      totalParcelsCount = length orders
  in
    -- --- Implementation of your conditions ---
    -- Condition 1: parcels > countThreshold -> true
    (totalParcelsCount > countThreshold)
    -- OR
    ||
    -- Condition 2: parcels <= countThreshold && totalWeight > weightThreshold -> true
    (totalParcelsCount <= countThreshold && totalWeight > weightThreshold)


-- | Implementation of the aggregate calculation for Yandex Courier Pickup
makePickupParams :: [OrdersForYandexCourierPickupItem] -> PickupParameters
makePickupParams items = 
    PickupParameters
    { -- FORMATTING FIX: Use "%.2f" to match the Yandex Regex [0-9]{1,2}
      -- This converts 0.0636 -> "0.06"
      volumeM3     = T.pack $ printf "%.2f" (max 0.25 totalVolumeM3)
    , weightG      = fromIntegral totalWeightG
    , requirements = Nothing
    }
  where
    -- 1. Calculate Total Weight (Int32 -> Int)
    totalWeightG = sum $ map oycpiWeight items
    -- 2. Calculate Total Volume in m3
    -- (Length * Width * Height) / 1,000,000
    totalVolumeM3 :: Double
    totalVolumeM3 = 
      let totalCm3 = 
            sum [ fromIntegral (oycpiLength * oycpiWidth * oycpiHeight) 
                  | OrdersForYandexCourierPickupItem {..} <- items 
                ]
      in fromIntegral totalCm3 / 1000000.0


-- | Selects the best available pickup window for a specific date
-- | Targeted preference: 12:00 - 18:00
selectPickupWindow :: [Ty.PickupOptionsRespItem] -> Day -> Maybe LocalTimeInterval
selectPickupWindow options targetDay =
    let 
        -- 1. Format date to "YYYY-MM-DD" to match Yandex string
        targetDateStr = targetDay
        
        -- 2. Find the entry for the requested day
        mDayItem = find (\item -> Ty.pickupLocalDate item == targetDateStr) options
    in mDayItem <&> \dayItem ->
          -- 3. Rank intervals by how many 'preferred hours' they contain
          -- (Preferred: 12, 13, 14, 15, 16, 17, 18)
          maximumBy (comparing (scoreInterval 12 18)) (Ty.pickupLocalTimeIntervals dayItem)


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