{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Yandex.CachedPickupPoints (storeDeliveryPoints) where

import Katip
import Data.Text (Text)
import Data.Traversable (for)
import Data.Time.Clock (diffUTCTime)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Concurrent.STM (atomically, modifyTVar')

import Text (tshow)
import App (AppM, _yandexPickupPoints, readTVarIO, currentTime)
import Infrastructure.Utils.Http (HttpError)
import API.WithField (WithField (..))
import Infrastructure.Services.Yandex (GeoId, PickupPoint, listPickupPoints, pprGeoId, defaultPickupPointsReq)

storeDeliveryPoints :: GeoId -> AppM (Either HttpError [WithField "metros" [Text] PickupPoint])
storeDeliveryPoints geoId = do
  stateTVar <- get
  pointsMap <- fmap _yandexPickupPoints $ readTVarIO stateTVar
  let maybePoints = M.lookup geoId pointsMap
  case maybePoints of 
    Nothing                    -> 
      fetchAdCache geoId 
    Just (cachedTime, points) -> do
      now <- currentTime
      if diffUTCTime now cachedTime < 864000 -- once in 10 days
      then fmap (const (Right points)) $ 
             $(logTM) InfoS $ 
               "Using cached Yandex pickup \
               \ points for geoId: " <> 
               ls (tshow geoId)
      else fetchAdCache geoId
    
fetchAdCache :: GeoId ->  AppM (Either HttpError [WithField "metros" [Text] PickupPoint])
fetchAdCache geoId = do
  eRes <- listPickupPoints defaultPickupPointsReq { pprGeoId = geoId }
  for eRes $ \points -> do
    now <- currentTime
    stateTVar <- get
    liftIO $ atomically $ modifyTVar' stateTVar $
      \s -> let old = _yandexPickupPoints s 
            in s { _yandexPickupPoints = 
                   M.insert geoId (now, points) old }
    return points