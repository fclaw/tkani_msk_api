{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}


module Infrastructure.Services.Google.Geocode where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Data.Text (Text)
import Data.Maybe (listToMaybe) -- Perfect for safely getting the first element

-- | Represents the latitude and longitude. Matches the innermost "location" object.
data GeoLocation = GeoLocation
    { lat :: Double
    , lng :: Double
    } deriving (Show, Eq, Generic)

-- Aeson can automatically derive the instance as JSON fields "lat" and "lng" match.
instance FromJSON GeoLocation

-- | Represents the geometry information.
data Geometry = Geometry
    { location     :: GeoLocation
    , locationType :: Text
    } deriving (Show, Eq, Generic)

-- We need a custom instance to map the JSON field "location_type" to "locationType".
instance FromJSON Geometry where
    parseJSON = withObject "Geometry" $ \v -> Geometry
        <$> v .: "location"
        <*> v .: "location_type"

-- | Represents a single geocoding result from the "results" array.
data GeocodingResult = GeocodingResult
    { formattedAddress :: Text
    , geometry         :: Geometry
    } deriving (Show, Eq, Generic)

-- Custom instance to map "formatted_address" to "formattedAddress".
instance FromJSON GeocodingResult where
    parseJSON = withObject "GeocodingResult" $ \v -> GeocodingResult
        <$> v .: "formatted_address"
        <*> v .: "geometry"

data Status = OK | ZERO_RESULTS | OVER_QUERY_LIMIT | REQUEST_DENIED | INVALID_REQUEST | UNKNOWN_ERROR
 deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { sumEncoding = UntaggedValue } ''Status)

-- | Represents the top-level response from the Geocoding API.
data GeocodingResponse = GeocodingResponse
    { result :: Maybe GeocodingResult -- CHANGED: From [GeocodingResult] to Maybe GeocodingResult
    , status :: Status
    } deriving (Show, Eq, Generic)

-- Because our Haskell type no longer perfectly mirrors the JSON (list vs. Maybe),
-- we write a custom FromJSON instance to bridge the gap.
instance FromJSON GeocodingResponse where
  parseJSON = withObject "GeocodingResponse" $ \v -> do
    -- 1. Parse the "results" field from JSON, which is still an array.
    resultsList <- v .: "results"

    -- 2. Safely take the first element of the list. 'listToMaybe' returns
    --    'Nothing' for an empty list and 'Just the_first_element' otherwise.
    --    This is the core of the implementation.
    let mostRelevantResult = listToMaybe (resultsList :: [GeocodingResult])

    -- 3. Parse the status field into our new 'Status' ADT.
    statusValue <- v .: "status"

    -- 4. Construct our record.
    pure $ GeocodingResponse mostRelevantResult statusValue

extractGeoCoordinates :: GeocodingResult -> (Double, Double)
extractGeoCoordinates GeocodingResult {..} = (lat (location  geometry),  lng (location  geometry))



--