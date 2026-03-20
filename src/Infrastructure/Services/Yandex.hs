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

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Services.Yandex 
       ( detectLocation
       , listPickupPoints
       , getNearestSource
       , calculatePrice
       , createOrder
       , fetchParcelLabel
       , fetchOrderParticulars
       , fetchTrackingUrl
       , initWarehouse
       , generateManifest
       , createShipment
       , getPickupOptions
       , fetchPickupStatus
       , fetchPickupPointAddress
       , cancelShipment
       , module Yandex.Types
       , PlatformId
       ) where

import Data.Aeson (toJSON, FromJSON, ToJSON)
import Katip (logTM, Severity(..), ls)
import Data.Text (Text, unpack)
import Data.Functor ((<&>))
import Data.Maybe (isJust, fromJust)
import Control.Monad (join)
import Data.List (minimumBy)
import Data.Ord  (comparing)
import Data.Time.Clock (diffUTCTime)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Wreq (defaults, auth, oauth2Bearer, postWith, manager)
import Control.Lens ((&), (?~), (.~))
import Data.Text.Encoding (encodeUtf8)
import Control.Exception (try)
import Network.HTTP.Client (HttpException, responseBody)

import App
import Text (tshow)
import API.WithField (WithField (..))
import TH.Location (currentModule)
import Infrastructure.Utils.Http
import Infrastructure.Services.Yandex.Geo
import Infrastructure.Services.Yandex.Types as Yandex.Types
import Infrastructure.Services.Yandex.Config hiding (Address)
import Infrastructure.Services.Yandex.Types.Enums (PaymentMethod (AlreadyPaid))
import Infrastructure.Services.Overpass.Geo (findNearestMetros)


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

type PlatformId = Text

-- | Core Function: Returns the closest point to a target from a given list
findClosestPoint :: GeoPoint -> [DropOffPoint] -> Maybe DropOffPoint
findClosestPoint _ [] = Nothing
findClosestPoint target points = Just $ minimumBy (comparing (haversineDist target . ppPosition)) points

getNearestSource :: AppM (Either HttpError (Maybe (Address, PlatformId)))
getNearestSource = do  
  stateTVar <- get
  maybePoints <- fmap _yandexDropOffPoints $ readTVarIO stateTVar
  case maybePoints of 
    Just (cachedTime, points) -> do
      now <- currentTime
      if diffUTCTime now cachedTime < 86400 -- once in 1 days
      then do
        cfg <- fmap _yandexConfig ask
        let officeGeoPoint = office cfg
        let target = findClosestPoint officeGeoPoint points
        pure $ Right $ join $ target <&> \t -> ppId t <&> \id -> (ppAddress t, id)
      else  do 
        ePoints <- fetchAndCache
        case ePoints of
          Left err -> 
            fmap (const (Left err)) $ 
              $(logTM) ErrorS $ 
                "error while fetching \
                \ Yandex pickup points: " <> 
                ls (tshow err)
          Right points -> do
            cfg <- fmap _yandexConfig ask
            let officeGeoPoint = office cfg
            let target = findClosestPoint officeGeoPoint points
            pure $ Right $ join $ target <&> \t -> ppId t <&> \id -> (ppAddress t, id)
    Nothing -> do 
      ePoints <- fetchAndCache
      case ePoints of
        Left err ->
          fmap (const (Left err)) $ 
            $(logTM) ErrorS $ 
              "error while fetching \
              \ Yandex pickup points: " <> 
              ls (tshow err)
        Right points -> do
          cfg <- fmap _yandexConfig ask
          let officeGeoPoint = office cfg
          let target = findClosestPoint officeGeoPoint points
          pure $ Right $ join $ target <&> \t -> ppId t <&> \id -> (ppAddress t, id)

fetchAndCache :: AppM (Either HttpError [DropOffPoint])
fetchAndCache = do
  cfg <- fmap _yandexConfig ask
  manager <- fmap _configHttpManager ask
  let (from, to)  = calcBoundingBox (office cfg) 2.0
  let minLat = min (latitude from) (latitude to)
  let maxLat = max (latitude from) (latitude to)
  let minLon = min (longitude from) (longitude to)
  let maxLon = max (longitude from) (longitude to)
  let _latitude   = CoordinateInterval minLat maxLat
  let _longitude  = CoordinateInterval minLon maxLon
  let req = defaultPickupPointsReq 
            { pprGeoId     = Nothing
            , pprLatitude  = Just _latitude
            , pprLongitude = Just _longitude
            }
  let url = show HTTPS <> unpack (apiUrl cfg) <> "/api/b2b/platform/pickup-points/list"
  let token = mkDefToken (apiKey cfg)
  eResp <- postReq manager url req [] (Just token)
  handleApiResponse @_ @PickupPointsResp $(currentModule) eResp $ \resp -> do
    stateTVar <- get
    cachedTime <- currentTime
    let points = pprPoints resp
    let pointsWithId = [ p | p <- points, isJust (ppId p)]
    fmap (const (Right pointsWithId)) $
      modifyTVarIO stateTVar $ \s -> 
        s { _yandexDropOffPoints = 
            Just (cachedTime, pointsWithId) 
          }

calculatePrice :: PriceCalculatorReq -> AppM (Either HttpError PriceCalculatorResp)
calculatePrice req = do
  cfg <- fmap _yandexConfig ask
  manager <- fmap _configHttpManager ask
  let url = show HTTPS <> unpack (apiUrl cfg) <> "/api/b2b/platform/pricing-calculator"
  let token = mkDefToken (apiKey cfg)
  postReq @PriceCalculatorResp manager url req [] (Just token)

createOrder :: YandexCreateOrderReq -> AppM (Either HttpError YandexCreateOrderResp)
createOrder req = do
  cfg <- fmap _yandexConfig ask
  manager <- fmap _configHttpManager ask
  let url = show HTTPS <> unpack (apiUrl cfg) <> "/api/b2b/platform/request/create"
  let token = mkDefToken (apiKey cfg)
  let header = ("Accept-Language", "ru")
  postReq @YandexCreateOrderResp manager url req [header] (Just token)
  
fetchParcelLabel :: YandexRequestId -> AppM (Either HttpException B.ByteString)
fetchParcelLabel orderId = do
  cfg <- fmap _yandexConfig ask
  mgr <- fmap _configHttpManager ask
  let url = show HTTPS <> unpack (apiUrl cfg) <> "/api/b2b/platform/request/generate-labels"
  let opts = 
        defaults 
        & auth ?~ oauth2Bearer (encodeUtf8 (apiKey cfg))
        & manager .~ Right mgr
  let req = YandexParcelLabelReq { requestIds = [orderId] }
  let handleResp (Left err) = Left err
      handleResp (Right response) = Right (BL.toStrict (responseBody response))
  fmap handleResp $ liftIO $ try @HttpException (postWith opts url (toJSON req))

fetchTrackingUrl :: YandexRequestId -> AppM TrackingUrl
fetchTrackingUrl orderId = do
  cfg <- fmap _yandexConfig ask
  manager <- fmap _configHttpManager ask
  let url = show HTTPS <> unpack (apiUrl cfg) <> "/api/b2b/platform/request/info"
  let token = mkDefToken (apiKey cfg)
  let params = [("request_id", orderId)]
  eResp <- getReq manager url params [] (Just token)
  handleApiResponse @_ @TrackingUrl $(currentModule) eResp $ pure

fetchOrderParticulars :: YandexRequestId -> AppM OrderParticulars
fetchOrderParticulars orderId =  do
  cfg <- fmap _yandexConfig ask
  manager <- fmap _configHttpManager ask
  let url = show HTTPS <> unpack (apiUrl cfg) <> "/api/b2b/platform/request/info"
  let token = mkDefToken (apiKey cfg)
  let params = [("request_id", orderId)]
  let userAgent = ("User-Agent", "Tkani-MSK-Internal-Sync-Service/1.0 (contact: fclaw007@gmail.com)") 
  eResp <- getReq manager url params [userAgent] (Just token)
  handleApiResponse @_ @OrderParticulars $(currentModule) eResp $ pure

-- post req helper 
makePostReq :: forall req resp . (ToJSON req, FromJSON resp) => Text -> req -> AppM (Either HttpError resp)
makePostReq urlPiece req = do
  cfg <- fmap _yandexConfig ask
  manager <- fmap _configHttpManager ask
  let url = show HTTPS <> unpack (apiUrl cfg) <> unpack urlPiece
  let token = mkDefToken (apiKey cfg)
  postReq @resp manager url req [] (Just token)

initWarehouse :: WarehouseCreateReq -> AppM (Either HttpError WarehouseCreateResp)
initWarehouse = makePostReq "/api/b2b/platform/warehouses/create"
{-# INLINE initWarehouse #-}

generateManifest :: ManifestReq -> AppM (Either Text B.ByteString)
generateManifest req = do
  cfg <- fmap _yandexConfig ask
  mgr <- fmap _configHttpManager ask
  let url = show HTTPS <> unpack (apiUrl cfg) <> "/api/b2b/platform/request/get-handover-act"
  let token = apiKey cfg
  let opts = 
        defaults 
        & auth ?~ oauth2Bearer (encodeUtf8 token) 
        & manager .~ Right mgr
  let handleResp (Left err) = Left (tshow err)
      handleResp (Right response) = Right (BL.toStrict (responseBody response))
  fmap handleResp $ liftIO $ try @HttpException (postWith opts url (toJSON req))

createShipment :: CreateShipmentReq -> AppM (Either HttpError CreateShipmentResp)
createShipment = makePostReq "/api/b2b/platform/pickups/create"
{-# INLINE createShipment #-}

getPickupOptions :: PickupOptionsReq -> AppM (Either HttpError PickupOptionsResp)
getPickupOptions = makePostReq "/api/b2b/platform/pickups/pickup-options"
{-# INLINE getPickupOptions #-}

fetchPickupStatus :: PickupStatusReq -> AppM (Either HttpError PickupStatusResp)
fetchPickupStatus = makePostReq "/api/b2b/platform/pickups/retrieve"
{-# INLINE fetchPickupStatus #-}

fetchPickupPointAddress :: PickupPointAddressReq -> AppM PickupPointAddressResp
fetchPickupPointAddress req = do
  eResp <- makePostReq "/api/b2b/platform/pickup-points/list" req
  handleApiResponse $(currentModule) eResp $ pure

cancelShipment :: CancelPickupReq -> AppM (Either HttpError ())
cancelShipment = makePostReq "/api/b2b/platform/pickups/cancel"
{-# INLINE cancelShipment #-}


