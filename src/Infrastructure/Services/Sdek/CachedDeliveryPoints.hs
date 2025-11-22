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
import Control.Monad (void)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (member)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically, readTVar)

import App (AppM, runAppM, sdekAccessToken, _sdekUrl, _metroCityCodes, _metroStations)
import API.Types (DeliveryPoint, ApiResponse)
import API.WithField (WithField)
import Text (recordLabelModifier)
import API.Types
import Infrastructure.Services.Sdek.Auth (getValidSdekToken)
import Infrastructure.Utils.Http
import TH.Location (currentModule)
import API.WithField (WithField (..))
import Infrastructure.Services.Overpass.Types (MetroStation)
import Infrastructure.Services.Overpass.Geo (findNearestMetros)


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
           allMetros <- fmap _metroStations $ liftIO $ atomically $ readTVar stateTVar
           return $ Right $ map (flip enrichWithMetro allMetros) sdekPoints
    else return $ Right $ map (WithField [] . transformSdekPoint) sdekPoints

enrichWithMetro :: SdekApiPoint -> [MetroStation] -> WithField "dpMetros" [Text] DeliveryPoint
enrichWithMetro sdekPoint allMetros = 
    let 
        -- 1. Extract lat/lon from the API object
        loc = sdekApiPointLocation sdekPoint
        lat = latitude loc
        lon = longitude loc
        
        -- 2. Perform the Pure Math (Find nearest stations)
        nearestMetros = findNearestMetros lat lon allMetros
        
        -- 3. Convert the rest of the object to your domain model
        basePoint = transformSdekPoint sdekPoint
    in 
    -- 4. Wrap it
    WithField nearestMetros basePoint