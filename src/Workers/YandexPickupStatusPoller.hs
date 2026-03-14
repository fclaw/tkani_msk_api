{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Workers.YandexPickupStatusPoller (runYandexPickupStatusPoller) where


import Katip
import Control.Monad (when, void)
import Data.Foldable (for_)
import Control.Monad.Reader.Class (ask)

import Text (tshow)
import App (AppM, _appDBPool, ChatKey(PICKUP))
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Database (fetchYandexPickupStatus, completeYandexPickup)
import Infrastructure.Services.Yandex.Types.Enums (PickupStatus(Completed))
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Infrastructure.Services.Yandex (fetchPickupStatus, PickupStatusReq (..), PickupStatusResp (..), PickupStatusRespItem (..))


runYandexPickupStatusPoller :: AppM ()
runYandexPickupStatusPoller = do
  $(logTM) InfoS "Yandex pickup poller run.."
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- fetchYandexPickupStatus pool
  case eDbRes of 
    Left err -> $(logTM) ErrorS $ "fetchYandexPickupStatus failure: " <> ls err
    Right vs -> do
      for_ vs $ \(pickupId, day) -> do
        eYaResp <- fetchPickupStatus $ PickupStatusReq pickupId
        case eYaResp of
          Left err -> $(logTM) ErrorS $ "fetchPickupStatus failure: " <> ls (tshow err)
          Right PickupStatusResp {pickups=PickupStatusRespItem{pickupStatus}} ->
            when (pickupStatus == Completed) $ do
              void $ completeYandexPickup pickupId pool
              let msg = escapeMarkdownV2 $ "Yandex courier pickup for " <> tshow day <> " has been completed"
              void $ sendOrEditTelegramMessage mempty msg PICKUP Nothing Nothing Nothing
  $(logTM) InfoS "Yandex pickup poller finished"