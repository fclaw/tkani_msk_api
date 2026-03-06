{-|

Module      : Infrastructure.Services.Yandex
Copyright   : (c) 2024-2025 Tkani Team
License     : MIT
Maintainer  : Sergey Yakovlev <

This module provides the infrastructure layer for communicating with the 
Yandex Delivery platform. It handles API requests for logistics orchestration, 
including:

  * Settlement identification and geocoding (geo_id detection).
  * Price estimation and delivery interval retrieval.
  * Order creation and management within the Yandex logistics ecosystem.

The module is designed to abstract the low-level HTTP communication (REST) 
and JSON serialization (Aeson) required by the Yandex B2B taxi/delivery 
endpoints.

See official documentation at: https://yandex.ru/support/delivery-profile/ru/api/
-}

{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Infrastructure.Services.Yandex (detectLocation, listPickupPoints, module Yandex.Types) where

import Katip (logTM, Severity(..), ls)
import Data.Text (Text, unpack)
import Data.Functor ((<&>))
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Monad.IO.Class (liftIO)

import Text (tshow)
import API.WithField (WithField (..))
import TH.Location (currentModule)
import Infrastructure.Utils.Http
import Infrastructure.Services.Yandex.Types as Yandex.Types
import Infrastructure.Services.Yandex.Config
import Infrastructure.Services.Overpass.Geo (findNearestMetros)
import App (AppM, _configHttpManager, Scheme (HTTPS), _yandexConfig, _metroStations, readTVarIO)


detectLocation :: LocationDetectReq -> AppM (Either HttpError [LocationDetectedVariant])
detectLocation req = do
  cfg <- fmap _yandexConfig ask
  manager <- fmap _configHttpManager ask
  let url = show HTTPS <> unpack (apiUrl cfg) <> "/api/b2b/platform/location/detect"
  let token = mkDefToken (apiKey cfg)
  eResp <- postReq manager url req [] (Just token)
  handleApiResponse @_ @LocationDetectResp $(currentModule) eResp $ \resp -> do
    fmap (const (Right (variants resp))) $ 
      $(logTM) InfoS $ ls $ "Yandex location detect response: " <> tshow resp


listPickupPoints :: PickupPointsReq -> AppM (Either HttpError [WithField "metros" [Text] PickupPoint])
listPickupPoints req = do
  cfg <- fmap _yandexConfig ask
  manager <- fmap _configHttpManager ask
  let url = show HTTPS <> unpack (apiUrl cfg) <> "/api/b2b/platform/pickup-points/list"
  let token = mkDefToken (apiKey cfg)
  eResp <- postReq manager url req [] (Just token)
  handleApiResponse @_ @PickupPointsResp $(currentModule) eResp $ \resp -> do
    let points = pprPoints resp -- Placeholder for actual response handling logic, replace with appropriate processing of pickup points data
    stateTVar <- get
    allMetros <- fmap _metroStations $ readTVarIO stateTVar
    pure $ Right $ points <&> \point -> 
      let GeoPoint {..} = ppPosition point
          metros = findNearestMetros latitude longitude allMetros
      in WithField metros point -- Assuming the response contains a list of pickup points and associated metro stations