{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Workers.SdekPickupAppStatusPoller (runSdekPickupAppStatusPoller) where

import Katip
import Data.Either (isLeft)
import Data.Foldable (for_)
import Control.Monad (void, when)
import Control.Monad.Reader.Class (ask)


import Text (tshow)
import API.Types (OrderStatus (..))
import App (AppM, _appDBPool, ChatKey(PICKUP))
import Infrastructure.Database (getAppStatusDetails, updatePickupAppStatus, updatePickedUpOrdersStatus)
import Infrastructure.Services.Sdek (getPickupApplicationStatus)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Infrastructure.Services.Sdek.Types.Courier (SdekPickupAppStatus (..), status, statusRank)


runSdekPickupAppStatusPoller :: AppM ()
runSdekPickupAppStatusPoller = do
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- getAppStatusDetails [READY_FOR_APPOINTMENT, APPOINTED_COURIER] pool
  case eDbRes of 
    Left err -> $(logTM) ErrorS $ ls $ "getAppStatusDetails resulted in error " <> tshow err
    Right Nothing -> pure ()
    Right (Just (id, app_uuid, oldStatus)) -> do
      eSdekRes <- getPickupApplicationStatus app_uuid
      case eSdekRes of
        Left err -> $(logTM) ErrorS $ ls $ "getPickupApplicationStatus resulted in error " <> tshow err
        Right appStatus -> do
          let newStatus = compareStatus oldStatus (status appStatus)
          case newStatus of
            Nothing -> do
              $(logTM) InfoS $ ls $ "pickup app status hasn't changed  " <> tshow oldStatus
            Just newStatus -> do
              $(logTM) InfoS $ ls $ "pickup app status changed from " <> tshow oldStatus <> " to " <> tshow newStatus
              let msg = escapeMarkdownV2 $ "pickup status changed from " <> tshow oldStatus <> " to " <> tshow newStatus
              void $ sendOrEditTelegramMessage mempty msg PICKUP Nothing Nothing Nothing

              -- CRITICAL: Update your database with the new status.
              -- This is a very important step missing from your original code.
              eDbRes <- updatePickupAppStatus id newStatus pool
              when (isLeft eDbRes) $ do
                $(logTM) ErrorS $ ls $ "updatePickupAppStatus resulted in error " <> tshow eDbRes
                let error = escapeMarkdownV2 $ "‼️ updatePickupAppStatus resulted in error " <> tshow eDbRes
                void $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
              
              for_ eDbRes $ \_ ->
                -- Add specific actions based on the *new, valid* status
                case newStatus of
                    APPOINTED_COURIER -> do
                      $(logTM) InfoS $ "Courier assigned"
                      -- Send notification to customer etc.
                      -- status hasn't changed
                      let msg = escapeMarkdownV2 $ "courier has been assigned to the order"
                      void $ sendOrEditTelegramMessage mempty msg PICKUP Nothing Nothing Nothing
                    DONE -> do
                      $(logTM) InfoS $ "Pickup completed"
                      -- Handle order fulfillment etc.
                      -- Send notification to customer etc.
                      updatePickedUpOrdersStatus id PickedUpByCourier pool
                      let msg = escapeMarkdownV2 $ "the fulfillment of the order has been completed"
                      void $ sendOrEditTelegramMessage mempty msg PICKUP Nothing Nothing Nothing
                    PROBLEM_DETECTED -> do
                      $(logTM) WarningS $ "Problem detected for pickup"
                    -- Send alert to admin channel.
                      updatePickedUpOrdersStatus id PickupFailed pool
                      let error = escapeMarkdownV2 $ "‼️ Problems detected. orders are marked as PickupFailed"
                      void $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
                    REMOVED -> do
                      $(logTM) WarningS $ "Pickup removed. Processing cancellation."
                    -- Trigger internal cancellation process if needed.
                    _ -> pure () -- Do nothing for other statuses, or add more logic           



          
-- | Compares two SdekPickupAppStatus values to determine if a valid forward transition
--   or a valid transition to a "problem" state has occurred.
--   Returns 'Just newStatus' if a valid transition, 'Nothing' if status hasn't changed
--   or if the transition is invalid (e.g., trying to go backwards).
compareStatus :: SdekPickupAppStatus -> SdekPickupAppStatus -> Maybe SdekPickupAppStatus
compareStatus oldStatus newStatus
  | oldStatus == newStatus = Nothing -- No change, so return Nothing.
  | newRank > oldRank      = Just newStatus -- Valid forward transition.
  -- Allow transitions to specific problem/terminal states even if rank is lower.
  -- This handles things like an order going from APPOINTED_COURIER to REMOVED.
  | newStatus `elem` [REMOVED, PROBLEM_DETECTED, INVALID] = Just newStatus
  | otherwise              = Nothing -- Invalid backward or sideways transition.
  where
    oldRank = statusRank oldStatus
    newRank = statusRank newStatus