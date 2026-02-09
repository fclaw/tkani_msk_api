{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Workers.SdekPickupAppStatusPoller (runSdekPickupAppStatusPoller) where

import Katip
import Control.Monad.Reader.Class (ask)


import Text (tshow)
import App (AppM, _appDBPool)
import Infrastructure.Database (getAppStatusDetails)
import Infrastructure.Services.Sdek (getPickupApplicationStatus)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Infrastructure.Services.Sdek.Types.Courier (SdekPickupAppStatus (READY_FOR_APPOINTMENT, APPOINTED_COURIER), status)


runSdekPickupAppStatusPoller :: AppM ()
runSdekPickupAppStatusPoller = return ()
--   cfg <- ask
--   let pool = _appDBPool cfg
--   eDbRes <- getAppStatusDetails [READY_FOR_APPOINTMENT, APPOINTED_COURIER] pool
--   case eDbRes of 
--     Left err -> $(logTM) ErrorS $ ls $ "getAppStatusDetails resulted in error " <> tshow err
--     Right (id, app_uuid, oldStatus) -> do
--       eSdekRes <- getPickupApplicationStatus app_uuid
--       case eSdekRes of
--         Left err -> $(logTM) ErrorS $ ls $ "getPickupApplicationStatus resulted in error " <> tshow err
--         Right appStatus -> do
--           let newStatus = compareStatus oldStatus (status appStatus)
--           case newStatus of
--             Nothing -> do
--               $(logTM) InfoS $ ls $ "pickup app status hasn't changed  " <> tshow oldStatus
--             Just newStatus -> do
--               $(logTM) InfoS $ ls $ "pickup app status changed from " <> tshow oldStatus <> " to " <> tshow newStatus
--               let msg = escapeMarkdownV2 $ "pickup status changed from " <> tshow oldStatus <> " to " <> tshow newStatus
--               void $ sendOrEditTelegramMessage mempty msg PICKUP Nothing Nothing Nothing
--               case newStatus of
--                 READY_FOR_APPOINTMENT -> 

          

-- compareStatus :: SdekPickupAppStatus -> SdekPickupAppStatus -> Maybe SdekPickupAppStatus
-- compareStatus oldStatus newStatus 
--   | oldStatus == newStatus = Nothing
--   | otherwise = Just newStatus