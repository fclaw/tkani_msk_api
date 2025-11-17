{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module Infrastructure.Services.Sdek.CachedDeliveryPoints (storeDeliveryPoints) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.TH
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Katip
import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async.Extra (mapConcurrentlyBounded)
import Data.Bifunctor (first)
import Control.Monad.Error.Class (throwError)
import Servant.Server.Internal.ServerError (err500)
import Data.Foldable (for_)
import Data.Traversable (for)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM (atomically, modifyTVar')
import Data.HashSet (member)

import App (AppM, runAppM, sdekAccessToken, _sdekUrl, _configYandexApiKey, _pointCache, currentTime, _metroCityCodes)
import API.Types (DeliveryPoint, ApiResponse)
import API.WithField (WithField)
import Text (recordLabelModifier)
import API.Types
import Infrastructure.Services.Sdek.Auth (getValidSdekToken)
import Infrastructure.Utils.Http
import TH.Location (currentModule)
import API.WithField (WithField (..))
import Infrastructure.Services.Yandex.Types


-- | Internal data type to decode the full delivery points response from SDEK.
-- We only define the fields we are interested in.
data SdekApiPoint = SdekApiPoint
  { sdekApiPointCode     :: Text
  , sdekApiPointName     :: Text
  , sdekApiPointWorkTime :: Text
  , sdekApiPointIsDressingRoom :: Bool
  , sdekApiPointLocation :: SdekApiLocation
  } deriving (Show, Generic)

data SdekApiLocation = SdekApiLocation
  { address_full :: Text
  , longitude    :: Double
  , latitude     :: Double
  } deriving (Show, Generic)

instance FromJSON SdekApiLocation
instance ToJSON SdekApiLocation

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sdekApiPoint" } ''SdekApiPoint)

takeEnd :: Int -> [a] -> [a]
takeEnd i xs
    | i <= 0 = []
    | otherwise = f xs (drop i xs)
    where f (x:xs) (y:ys) = f xs ys
          f xs _ = xs

-- | A pure function that transforms the SDEK-specific data structure
--   into our clean, internal 'DeliveryPoint' ADT.
transformSdekPoint :: SdekApiPoint -> DeliveryPoint
transformSdekPoint sp =
  let
    shortAddress = (T.intercalate ", " . takeEnd 2 . T.splitOn ", " . address_full . sdekApiPointLocation) sp
    buttonText = sdekApiPointName sp <> " - " <> shortAddress
    fittingRoomText = if sdekApiPointIsDressingRoom sp then "\n👕 Есть примерочная" else mempty
    messageText = T.unlines
      [ "📍 **Пункт СДЭК '" <> sdekApiPointName sp <> "'**"
      , "**Адрес:** " <> address_full (sdekApiPointLocation sp)
      , "**Часы работы:** " <> sdekApiPointWorkTime sp
      , fittingRoomText
      ]
  in
  DeliveryPoint
    { dpCode            = "sdek_" <> sdekApiPointCode sp
    , dpName            = sdekApiPointName sp
    , dpWorkTime        = sdekApiPointWorkTime sp
    , dpHasDressingRoom = sdekApiPointIsDressingRoom sp
    , dpLocation        = PointLocation
        { locAddressFull = (address_full . sdekApiPointLocation) sp
        , locLongitude   = (longitude . sdekApiPointLocation) sp
        , locLatitude    = (latitude . sdekApiPointLocation) sp
        }
    , dpDisplay = DisplayInfo
        { diButtonText  = buttonText
        , diMessageText = messageText
        }
    }

max_pints_threshold :: Int
max_pints_threshold = 80

storeDeliveryPoints :: Int -> AppM (ApiResponse [WithField "dpMetros" [Text] DeliveryPoint])
storeDeliveryPoints cityCode = do
  $(logTM) InfoS $ logStr $ "Found SDEK city code " <> T.pack (show cityCode) <> ". Fetching points."
  url <- fmap (T.unpack . _sdekUrl) ask
  let pointsUrl = "https://" <> url <> "/v2/deliverypoints"
  let pointsParams = [("city_code", T.pack $ show cityCode), ("type", "PVZ")]
  let pointsReq = getValidSdekToken >>= (_getReq' pointsUrl pointsParams . Just . sdekAccessToken)
  ePoints <- makeRequestWithRetries @[SdekApiPoint] (Just (void $ getValidSdekToken)) pointsReq
  handleApiResponse @_ @[SdekApiPoint] $(currentModule) ePoints $ \sdekPoints -> do
    -- Transform the result (only runs on success of the second call)
    $(logTM) InfoS $ logStr $ "Successfully fetched " <> T.pack (show (length sdekPoints)) <> " points."
    -- metro stations
    appEnv <- ask
    stateTVar <- get
    let metroCities = _metroCityCodes appEnv
    if cityCode `member` metroCities && 
       length sdekPoints > max_pints_threshold
    then do
          let fun = (fmap (first (T.pack. show)) . runAppM appEnv stateTVar . enrichWithMetro)
          freshPoints <- liftIO $ fmap (first mkError . sequenceA) $ mapConcurrentlyBounded 20 fun sdekPoints
          for freshPoints $ \ps -> do
            now <- currentTime
            liftIO $ atomically $ modifyTVar' stateTVar $
              \s -> s { _pointCache = HM.insert cityCode (now, ps) (_pointCache s) }
            return ps
    else return $ Right $ map (WithField [] . transformSdekPoint) sdekPoints

enrichWithMetro :: SdekApiPoint -> AppM (WithField "dpMetros" [Text] DeliveryPoint)
enrichWithMetro point = do
  yandexApiKey <- fmap _configYandexApiKey ask
  let lon = longitude (sdekApiPointLocation point)
  let lat = latitude (sdekApiPointLocation point)
  let params = 
        [ ("apikey", yandexApiKey)
        , ("geocode", T.pack $ show lon <> "," <> show lat)
        , ("kind", "metro")
        , ("results", "3")
        , ("format", "json")
        ]
  eResult <- getReq @YandexResponse "https://geocode-maps.yandex.ru/1.x/" params Nothing
  case eResult of
    Left err -> do
      $(logTM) ErrorS $ "Yandex API call failed: " <> ls (show err)
      throwError err500
    Right yandexResponse -> do
      -- The parsing is already done by 'getReq'!
      -- We just use our pure helper function to extract the names.
      let metroNames = extractMetroNames yandexResponse
      return $ WithField metroNames (transformSdekPoint point)