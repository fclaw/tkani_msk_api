{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RecordWildCards  #-}


module Infrastructure.Services.Sdek.CachedCityCodes (fetchCityCodeForPvz) where

import Katip
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Control.Concurrent.STM (atomically, modifyTVar', TVar)


import Text (tshow)
import App (AppM, readTVarIO, _cityCodeByPVZCache, CityCodeByPVZCache (..), spvzCityName, scCode, SdekPvzInfo (..), SdekCity (..))
import qualified Infrastructure.Services.Sdek as Sdek (getDeliveryPointByCode, getCityByName)
import Infrastructure.Services.Sdek.Types (DeliveryPoint (..), SdekCityWithCode (..))



-- cache: (Int, [Text]), traverse over Text to find code??
fetchCityCodeForPvz :: Text -> AppM (Maybe Int)
fetchCityCodeForPvz pvzCode = do
  stateTVar <- get
  CityCodeByPVZCache {..} <- fmap _cityCodeByPVZCache $ readTVarIO stateTVar
  pvzMap <- readTVarIO pvzCacheVar
  case Map.lookup pvzCode pvzMap of
    -- === CACHE HIT (Partial or Full) ===
    Just pvzInfo -> do
      let cityName = spvzCityName pvzInfo
      $(logTM) DebugS $ ls $ "PVZ Cache HIT for " <> pvzCode <> ". City: " <>  cityName
      -- Now, look up the city in the City Cache
      cityMap <- readTVarIO cityCacheVar
      case Map.lookup (T.toLower cityName) cityMap of
        -- Full cache hit!
        Just city -> pure $ Just (scCode city)
        -- PVZ was cached, but its city wasn't. Fetch city.
        Nothing   -> fetchAndCacheCityByName cityName cityCacheVar
    -- === CACHE MISS (PVZ not found) ===
    Nothing -> do
      $(logTM) DebugS $ "PVZ Cache MISS for " <> ls pvzCode <> ". Fetching from SDEK API."      
      ePointList <- Sdek.getDeliveryPointByCode pvzCode
      case ePointList of
        Right [DeliveryPoint {..}] -> do -- We expect exactly one result
          -- Update both caches atomically
          liftIO $ atomically $ do
            let newPvzInfo = SdekPvzInfo pvzCode dpCityName
            modifyTVar' pvzCacheVar (Map.insert pvzCode newPvzInfo)
                        
            let newCity = SdekCity dpCityCode dpCityName
            modifyTVar' cityCacheVar (Map.insert (T.toLower dpCityName) newCity)
                    
          fmap (const (Just dpCityCode)) $ $(logTM) DebugS $ ls $ "Cached new PVZ " <> pvzCode <> " and City " <> dpCityName
        
        Right _ -> fmap (const Nothing) $ $(logTM) ErrorS $ "SDEK API returned zero or multiple points for code " <> ls pvzCode

        Left err -> fmap (const Nothing) $ $(logTM) ErrorS $ ls $ "SDEK API error fetching PVZ " <> pvzCode <> ": " <> tshow err

fetchAndCacheCityByName :: Text -> TVar (Map.Map Text SdekCity) -> AppM (Maybe Int)
fetchAndCacheCityByName cityName cityCacheVar = do
  $(logTM) DebugS $ "City Cache MISS for " <> ls cityName <> ". Fetching from SDEK API."
    
  -- Call the SDEK API for city info
  eCityList <- Sdek.getCityByName cityName
    
  case eCityList of
    Left err -> fmap (const Nothing) $ $(logTM) ErrorS $ "SDEK API error fetching city " <> ls cityName <> ": " <> ls (show err)
    Right cities ->
      -- SDEK can return multiple cities with the same name. We take the first.
      case listToMaybe cities of
        Nothing -> fmap (const Nothing) $ $(logTM) WarningS $ "SDEK API found no city named " <> ls cityName
        Just SdekCityWithCode {..} -> do
          -- Update the cache
          let scCode = sccCode
          let scName = sccCity
          liftIO $ atomically $ modifyTVar' cityCacheVar (Map.insert (T.toLower cityName) SdekCity {..})
          fmap (const (Just scCode)) $ $(logTM) DebugS $ "Cached new city: " <> ls cityName <> " with code " <> ls (show scCode)