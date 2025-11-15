{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}


module Infrastructure.Services.Sdek (getDeliveryPoints) where

import Data.Text (Text)
import Control.Monad.IO.Class
import Katip
import Data.Aeson
import Data.Aeson.TH
import Text (recordLabelModifier)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Control.Monad (forM_)
import Control.Monad.Reader.Class (ask)

import Types (AppM, sdekAccessToken, _sdekUrl)
import API.Types
import Infrastructure.Utils.Http
import Infrastructure.Services.Sdek.Auth (getValidSdekToken)
import TH.Location (currentModule)


-- | Internal data type to decode the city search response from SDEK.
newtype SdekCity = SdekCity { code :: Int }
  deriving (Show)

-- | Manual FromJSON instance to extract the "code" field from the JSON object.
instance FromJSON SdekCity where
  parseJSON = withObject "SdekCity" $ \v -> do
    -- 'v' is the JSON object.
    -- The '.:' operator looks up the "code" key and parses its value as an Int.
    cityCode <- v .: "code"
    -- We then wrap the resulting Int in our SdekCity constructor.
    pure $ SdekCity cityCode


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

getDeliveryPoints :: Text -> AppM (ApiResponse [DeliveryPoint])
getDeliveryPoints city = do
  token <- getValidSdekToken
  url <- fmap (T.unpack . _sdekUrl) ask 

  -- Step 2: Find the SDEK city code.
  $(logTM) InfoS $ logStr $ "Fetching SDEK city code for " <> city
  let cityUrl = "https://" <> url <> "/v2/location/cities"
  let cityParams =
        [ ("country_codes", "RU")  -- THE FIX: Limit search to Russia
        , ("city", city)           -- The city name to search for
        , ("size", "1")            -- Optional but good practice: we only need one result
        , ("lang", "rus")          -- Optional but good practice: ensure Russian response
        ]
  
  eCities <- getReq @[SdekCity] cityUrl cityParams (Just ((sdekAccessToken token)))
  handleApiResponse @_ @[SdekCity] $(currentModule) eCities $ \case
    [] -> do
      $(logTM) InfoS $ logStr $ "SDEK city not found for: " <> city
      pure $ Right [] -- Return an empty list, which is a valid success case.
    (_:_:_) -> do 
      $(logTM) InfoS $ logStr $ "SDEK city not found (no exact match): " <> city
      pure $ Right []
    (firstCity:_) -> do
      -- Step 3: Use the city code to fetch the list of delivery points.
      let cityCode = code firstCity
      $(logTM) InfoS $ logStr $ "Found SDEK city code " <> T.pack (show cityCode) <> ". Fetching points."
      let pointsUrl = "https://" <> url <> "/v2/deliverypoints"
      let pointsParams = [("city_code", T.pack $ show cityCode), ("type", "PVZ")]
      ePoints <- getReq @[SdekApiPoint] pointsUrl pointsParams (Just ((sdekAccessToken token)))
      handleApiResponse @_ @[SdekApiPoint] $(currentModule) ePoints $ \sdekPoints -> do
        -- Transform the result (only runs on success of the second call)
        $(logTM) InfoS $ logStr $ "Successfully fetched " <> T.pack (show (length sdekPoints)) <> " points."
        let points = map transformSdekPoint sdekPoints
        forM_ points $ \point -> $(logTM) DebugS $ logStr $ "Successfully fetched point: " <> T.pack (show point)
        pure $ Right $ points