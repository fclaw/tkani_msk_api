{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.Shelf.GetSdekPreferredPoint(handler) where

import Katip
import Data.List (find)
import Data.Int (Int64)
import Control.Monad (when, void)
import Data.Maybe (isNothing)
import Control.Monad.Reader.Class (ask)

import Text (tshow)
import App (AppM, SdekPointCode (..), _appDBPool)
import API.Types (ApiResponse, PreferredSdekPointWithAddress (..))
import Infrastructure.Database (fetchPreferredSdekPoint)
import Infrastructure.Services.Sdek.CachedDeliveryPointsCodes (fetchCodes)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: Int64 -> AppM (ApiResponse (Maybe PreferredSdekPointWithAddress))
handler userId = do
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- fetchPreferredSdekPoint userId pool
  case eDbRes of
    Left err -> fmap undefined $ $(logTM) ErrorS $ "db error in Handlers.GetSdekPreferredPoint: " <> ls (tshow err)
    Right Nothing -> return $ Right Nothing
    Right (Just code) -> do
      codes <- fetchCodes
      let address =
            fmap spcAddress $
              flip find codes $
                \SdekPointCode {..} -> 
                   spcCode == code       
      return $ Right $ fmap (PreferredSdekPointWithAddress code) address