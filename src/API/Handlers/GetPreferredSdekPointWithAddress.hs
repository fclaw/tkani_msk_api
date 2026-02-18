{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.GetPreferredSdekPointWithAddress(handler) where

import Katip
import Data.List (find)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
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
    Left err -> fmap undefined $ $(logTM) ErrorS $ "db error in Handlers.GetPreferredSdekPointWithAddress: " <> ls (tshow err)
    Right Nothing -> return $ Right Nothing
    Right (Just code) -> do
      codes <- fetchCodes
      let address = 
            spcAddress $
              fromMaybe undefined $ 
                flip find codes $ 
                  \SdekPointCode {..} -> 
                    spcCode == code
      return $ Right $ Just $ PreferredSdekPointWithAddress code address