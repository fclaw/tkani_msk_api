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
import Data.Foldable (for_)
import Data.Time.Calendar (addDays)
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay, Day)

import Text (tshow)
import App (AppM, _yandexConfig, _appDBPool, ChatKey(PICKUP), readTVarIO, _yandexWarehouseId)
import API.Types (OrderStatus (ScheduledForPickup))
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Yandex.Shipment
import Infrastructure.Services.Yandex.Error (getError, getHttpException)
import Infrastructure.Services.Yandex (generateManifest, createShipment, getPickupOptions)
import qualified Infrastructure.Services.Yandex.Types as Ty (PickupOptionsRespItem (..))
import Infrastructure.Services.Yandex.Types (PickupOptionsResp (..), PickupOptionsReq (..), ManifestReq (..), CreateShipmentResp (..), CreateShipmentReq (..), PlatformStationId (..))
import Infrastructure.Services.Yandex.Config (pickupParcels, pickupWeight, pickupWindow, fromHour, toHour)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, sendDocument)
import Infrastructure.Database (fetchOrdersForYandexCourierPickup, linkOrdersToPickup, OrdersForYandexCourierPickupItem (..))


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
    Right Nothing   -> fmap (const False) $ $(logTM) InfoS $ "No new paid orders to schedule."
    Right (Just (_, [])) -> fmap (const False) $ $(logTM) InfoS $ "No new paid orders to schedule."
    Right (Just (pickupId, orders)) -> do
      -- We have enough orders to schedule a pickup
      $(logTM) InfoS "Scheduling YANDEX courier pickup for orders..."          
      -- ... (the rest of your logic to call the SDEK API) ...
      $(logTM) InfoS $ ls $ "Found " <> tshow (length orders) <> " orders. Scheduling courier..."
      let manifestReq = ManifestReq $ map oycpiRequestId orders
      eManifest <- generateManifest manifestReq
      case eManifest of
        Left err -> do
          $(logTM) ErrorS $ ls $ "generateManifest failed: " <> tshow err
          let error = escapeMarkdownV2 $ "‼️ YANDEX: Error in calling generateManifest: " <> tshow err
          fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
        Right pdfBytes -> do
          -- send manifest to the PICKUP channel
          let caption = 
                escapeMarkdownV2 $ 
                  "YANDEX courier call has \
                  \ been registered for " <>
                  (tshow (addDays 1 today))
          let filename = "pickup-manifest-" <> tshow today <> ".pdf"
          -- 2. Call the new service function
          void $ sendDocument PICKUP caption filename pdfBytes "application/pdf"
          -- link
          linkOrdersToPickup pickupId (map oycpiOrderId orders) pool
          fmap (const True) $ $(logTM) InfoS $ "Successfully sent YANDEX pickup manifest for " <> ls (tshow today)