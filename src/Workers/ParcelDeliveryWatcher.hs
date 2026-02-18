{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Workers.ParcelDeliveryWatcher (runParcelDeliveryWatcher) where


import Katip
import Control.Monad (void, when)
import qualified Data.Text as T
import Data.Foldable (for_)
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool, ChatKey(PICKUP))
import Infrastructure.Database (fetchLostParcels)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)


runParcelDeliveryWatcher :: AppM ()
runParcelDeliveryWatcher = do
  $(logTM) InfoS "ParcelDeliveryWatcher started."
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- fetchLostParcels pool
  for_ eDbRes $ \orderIds ->
    when(length orderIds > 0) $ do
      let ordersList = T.unlines orderIds
      let warning = 
            escapeMarkdownV2 $ 
              "‼️ the following orders are lost \
              \ during the transfer to \
              \ the warehouse by courier:\n" <>
              ordersList
      void $ sendOrEditTelegramMessage mempty warning PICKUP Nothing Nothing Nothing