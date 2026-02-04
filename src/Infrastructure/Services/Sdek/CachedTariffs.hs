{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Infrastructure.Services.Sdek.CachedTariffs (getTariffs) where

import Katip
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Traversable (for)
import Data.Time.Clock (diffUTCTime)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Concurrent.STM (atomically, modifyTVar')

import App (AppM, NormalizedRoute (..), extractFromMaybe, readTVarIO, _sdekTariffs, currentTime)
import Infrastructure.Utils.Http (HttpError)
import Infrastructure.Services.Sdek (getAvailableTariffs)
import Infrastructure.Services.Sdek.Types (Location, mkLocation, AvailableTariffsResponse (..), tariff_code)
import Infrastructure.Services.Sdek.CachedCityCodes (fetchCityCodeForPvz)


storeTariffs :: Location -> Location -> AppM (Either HttpError (Maybe [Int]))
storeTariffs locFrom locTo = do
  stateTVar <- get
  tariffsMap <- fmap _sdekTariffs $ readTVarIO stateTVar
  let maybeTariff = M.lookup (NormalizedRoute (locFrom, locTo)) tariffsMap
  case maybeTariff of 
    Nothing                    -> 
      fetchAdCache locFrom locTo 
    Just (cachedTime, tariffs) -> do
      now <- currentTime
      if diffUTCTime now cachedTime < 864000 -- once in 10 days
      then return $ Right $ Just tariffs
      else fetchAdCache locFrom locTo

fetchAdCache locFrom locTo = do
  stateTVar <- get
  eSdekResp <- getAvailableTariffs locFrom locTo
  for eSdekResp $ \res@AvailableTariffsResponse {..} -> do
    if isJust atrErrors then 
      fmap (const Nothing) $ 
        $(logTM) ErrorS $ 
          "getAvailableTariffs returns error: " <> 
          ls (show atrErrors)
    else do
      now <- currentTime
      let tariffs = [ tariff_code t | t <- atrTariffCodes ]
      let key = NormalizedRoute (locFrom, locTo)
      let val = (now, tariffs)
      liftIO $ atomically $ modifyTVar' stateTVar $
        \s -> let old = _sdekTariffs s 
              in s { _sdekTariffs = 
                     M.insert key val old }
      return $ Just tariffs

-- the list of available tariff 
getTariffs :: Text -> Text -> AppM (Either HttpError (Maybe [Int]))
getTariffs fromWarehouse toWarehouse = do 
  maybeFrom <- fetchCityCodeForPvz fromWarehouse
  maybeTo   <- fetchCityCodeForPvz toWarehouse
  case (maybeFrom, maybeTo) of
    (Just from, Just to) -> 
      storeTariffs (mkLocation from) (mkLocation to)
    _                    ->
      fmap (const (Right Nothing)) $
        $(logTM) WarningS $ 
          "SDEK API found no city. either " <> 
          ls fromWarehouse <> ", or " <> 
          ls toWarehouse <> " is not available"
