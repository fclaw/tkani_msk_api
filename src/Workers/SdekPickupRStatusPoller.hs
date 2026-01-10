{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Workers.SdekPickupRStatusPoller (pickupStatusPoller) where


import Katip
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.UUID (UUID)
import Data.Either (isLeft, fromLeft)
import Data.Foldable (for_)
import Control.Monad (void, when)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import Data.Int (Int64)
import qualified Data.HashMap.Strict as HM
import Data.Time.LocalTime (localTimeOfDay, TimeOfDay(..), utcToLocalTime, zonedTimeToUTC, TimeZone(..))
import Data.Time (getZonedTime, localDay, formatTime, defaultTimeLocale, Day)


import App (AppM, _appDBPool, render, ChatKey (ORDER), currentTime)
import Infrastructure.Database (getPendingPickupRequests)
import Text (tshow)
import TH.Location (currentModule)
import Concurrency (pooledForConcurrentlyN)
import Infrastructure.Services.Sdek (getPickupApplicationByUUID)
import Infrastructure.Services.Sdek.Types.State
import Infrastructure.Services.Sdek.Types.Courier (SdekPickupApplicationResponse (..))
import Infrastructure.Services.Sdek.Types.Error (SdekErrorDetail (..))
import Infrastructure.Database (updatePickupStatus, recordCourierPickupFailureExt)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)



uncarry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncarry4 f (x1, x2, x3, x4) = f x1 x2 x3 x4

-- | Formats a Day into a "YYYY_MM_DD" Text string.
formatDayForTag :: Day -> Text
formatDayForTag = T.pack . formatTime defaultTimeLocale "%Y_%m_%d"


pickupStatusPoller :: AppM ()
pickupStatusPoller = do
  -- Implementation goes here
  $(logTM) InfoS "Polling for SDEK courier statuses..."
  pool <- fmap _appDBPool ask
  -- Your logic to poll SDEK courier statuses and update the database
  -- STEP 1: FETCH ALL PENDING RECORDS
  eRequestUuids <- getPendingPickupRequests pool
  case eRequestUuids of
    Left dbErr -> $(logTM) ErrorS $ ls $ "DB error fetching pending pickups: " <> tshow dbErr
    Right uuids -> do
      $(logTM) InfoS $ ls $ "Found " <> tshow (length uuids) <> " SDEK pickups to check."
      -- STEP 2 (to be implemented):
      -- For each UUID, call the SDEK "Get Pickup Application by UUID" endpoint
      -- and handle the response (update DB, revert on error, etc.)
      -- Use your bounded concurrent mapper for efficiency.
      pickedUpOrders <- pooledForConcurrentlyN 3 uuids (uncarry4 checkSinglePickupStatus)
      let successfulPickups = catMaybes pickedUpOrders
      $(logTM) InfoS $ ls $ "Successfully confirmed " <> tshow (length successfulPickups) <> " pickups."
      when(not (null successfulPickups)) $ do

        let orders = zipWith (\(oid, tn) idx -> (oid, tn, idx + 1)) successfulPickups [0..]
        let msk = TimeZone (3 * 60) False "MSK"
        now <- liftIO $ getZonedTime
        let mskTime = utcToLocalTime msk (zonedTimeToUTC now)
        let (TimeOfDay hour _ _) = localTimeOfDay mskTime
        let day = localDay mskTime

        let payload = 
              HM.fromList 
              [("date", formatDayForTag day),
               ("orders", 
                T.intercalate "\n"
                (map (\(oid, tn, idx) ->
                  tshow idx <> ". " <>  
                  "Order ID: " <> oid <> 
                  ", Tracking Number: " <> tn) 
                 orders))]
        message <- fmap escapeMarkdownV2 $ render ($currentModule <> ".Shipment") payload
        void $ sendOrEditTelegramMessage mempty message ORDER Nothing Nothing Nothing

checkSinglePickupStatus :: UUID -> Int64 -> Text -> Text -> AppM (Maybe (Text, Text))
checkSinglePickupStatus uuid msgId orderId trackingNumber = do
  -- Placeholder for the actual implementation
  $(logTM) InfoS $ ls $ "Checking status for SDEK pickup UUID: " <> tshow uuid
  -- Here you would call the SDEK API and update the database accordingly
  eResponse <- getPickupApplicationByUUID uuid
  case eResponse of
    Left apiErr -> fmap (const Nothing) $ $(logTM) ErrorS $ ls $ "Error fetching pickup status for UUID " <> tshow uuid <> ": " <> tshow apiErr
    Right response -> do
      pool <- fmap _appDBPool ask  
      $(logTM) InfoS $ ls $ "Successfully fetched pickup status for UUID " <> tshow uuid <> ": " <> tshow response
      case state response of
        Successful -> do
          -- THE HAPPY PATH
          $(logTM) InfoS $ ls $ "SDEK courier pickup " <> tshow uuid <> " is confirmed."
          -- Update the pickup status to 'successful'
          eDbRes <- updatePickupStatus uuid "successful" pool
          when (isLeft eDbRes) $
            $(logTM) ErrorS $ ls $ 
              "Failed to update DB status for pickup " <> 
              tshow uuid <> ": " <> 
              tshow (fromLeft undefined eDbRes)
          return $ Just (orderId, trackingNumber)     
        Waiting -> do
          -- STILL PENDING
           fmap (const Nothing) $ $(logTM) InfoS $ ls $ "SDEK courier pickup " <> tshow uuid <> " is still waiting."
          -- No DB change needed, we'll check again on the next loop.
    
        Invalid -> do
          -- THE FAILURE PATH (YOUR CRUCIAL STEP)
          let errorDetails = fromMaybe [] (errors response)
          let errorMsg = T.intercalate ", " (map message errorDetails)

          fmap (const Nothing) $ $(logTM) ErrorS $ ls $ "SDEK courier pickup " <> tshow uuid <> " FAILED with status INVALID. Reason: " <> errorMsg

          -- Run the Revert Transaction
          eDbRes <- recordCourierPickupFailureExt orderId uuid errorMsg pool
          when (isLeft eDbRes) $
            $(logTM) ErrorS $ ls $ 
              "Failed to update DB status (recordCourierPickupFailure) for pickup " <> 
              tshow uuid <> ": " <> 
              tshow (fromLeft undefined eDbRes)
        
          -- Send an alert to the admin
          for_ eDbRes $ const $ do
            message <- fmap escapeMarkdownV2 $ render ($currentModule <> ".Error") $ HM.fromList [("uuid", tshow uuid)]
            void $ sendOrEditTelegramMessage mempty message ORDER Nothing (Just msgId) Nothing   
          return Nothing   

        _ -> fmap (const Nothing) $ $(logTM) WarningS $ ls $ "Received unknown status for pickup " <> tshow uuid <> ": " <> tshow (state response)