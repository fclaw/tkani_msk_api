{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE DataKinds  #-}


module Infrastructure.Services.Sdek (getDeliveryPoints) where

import Data.Text (Text)
import Control.Monad.IO.Class
import Katip
import Data.Aeson
import Data.Aeson.TH
import Text (recordLabelModifier)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Control.Monad (forM_, void)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Concurrent.STM (atomically, readTVar)
import Data.Time (UTCTime, diffUTCTime)
import qualified Data.HashMap.Strict as HM

import App (AppM, sdekAccessToken, _sdekUrl, _pointCache, currentTime)
import API.Types
import Infrastructure.Utils.Http
import Infrastructure.Services.Sdek.Auth (getValidSdekToken)
import TH.Location (currentModule)
import API.WithField (WithField (..))
import Infrastructure.Services.Sdek.CachedDeliveryPoints (storeDeliveryPoints)


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

getDeliveryPoints :: Text -> AppM (ApiResponse [WithField "dpMetros" [T.Text] DeliveryPoint])
getDeliveryPoints city = do
  url <- fmap (T.unpack . _sdekUrl) ask
  -- Step 1: Find the SDEK city code.
  $(logTM) InfoS $ logStr $ "Fetching SDEK city code for " <> city
  let cityUrl = "https://" <> url <> "/v2/location/cities"
  let cityParams =
        [ ("country_codes", "RU")  -- THE FIX: Limit search to Russia
        , ("city", city)           -- The city name to search for
        , ("size", "1")            -- Optional but good practice: we only need one result
        , ("lang", "rus")          -- Optional but good practice: ensure Russian response
        ]
  let cityReq = getValidSdekToken >>= (_getReq' cityUrl cityParams . Just . sdekAccessToken)
  eCities <- makeRequestWithRetries @[SdekCity] (Just (void $ getValidSdekToken)) cityReq
  handleApiResponse @_ @[SdekCity] $(currentModule) eCities $ \case
    [] -> do
      $(logTM) InfoS $ logStr $ "SDEK city not found for: " <> city
      pure $ Right [] -- Return an empty list, which is a valid success case.
    (_:_:_) -> do 
      $(logTM) InfoS $ logStr $ "SDEK city not found (no exact match): " <> city
      pure $ Right []
    (firstCity:_) -> do
        stateTVar <- get
        now <- currentTime
        -- Read the current cache content atomically
        currentCache <- fmap _pointCache $ liftIO $ atomically $ readTVar stateTVar
        -- Check if a valid, fresh entry exists
        case HM.lookup (code firstCity) currentCache of
          Just (timestamp, cachedPoints) | isFresh now timestamp -> do
            -- CACHE HIT
            $(logTM) InfoS $ "Cache hit for city: " <> ls city
            return $ Right cachedPoints
          _ -> do
            $(logTM) InfoS $ "Cache miss for city: " <> ls city <> ". Fetching from APIs..."
            storeDeliveryPoints $ code firstCity


-- A helper to define what "fresh" means (e.g., 6 hours)
isFresh :: UTCTime -> UTCTime -> Bool
isFresh now prev = let sixHours = 6 * 60 * 60 in diffUTCTime now prev < sixHours