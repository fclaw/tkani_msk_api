{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module API.Handlers.Shelf.GetShelfPersonalInfo (handler) where


import Katip
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Traversable (for)
import Data.Bifunctor (first)
import Control.Monad.Reader.Class (ask)

import App (AppM, SdekPointCode (..), _appDBPool)
import Infrastructure.Database (getShelfPersonalInfo)
import API.Types(ApiResponse, ShelfPersonalInfo (..), mkError)
import Infrastructure.Services.Sdek.CachedDeliveryPointsCodes (fetchCodes)


handler :: Int64 -> AppM (ApiResponse ShelfPersonalInfo)
handler userId = do
  pool <- fmap _appDBPool ask
  eDbRes <- getShelfPersonalInfo userId pool
  fmap (first (const (mkError "server error"))) $ 
    for eDbRes $ \(initials, phone, sdekPointCode) -> do
      codes <- fetchCodes
      let isCodeValid =
            isJust $
              flip find codes $
                \SdekPointCode {..} ->
                  isJust sdekPointCode &&
                  Just spcCode == sdekPointCode
      let code | isCodeValid = sdekPointCode
               | otherwise = fmap ((<>) "⚠️ Недействительный код пункта: ") sdekPointCode
      return $ ShelfPersonalInfo initials phone code
        