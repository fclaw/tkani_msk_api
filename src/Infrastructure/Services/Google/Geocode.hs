{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}


module Infrastructure.Services.Google.Geocode where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

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

-- | Represents the top-level response from the Geocoding API.
data GeocodingResponse = GeocodingResponse
    { results :: [GeocodingResult]
    , status  :: Text -- e.g., "OK", "ZERO_RESULTS"
    } deriving (Show, Eq, Generic)

-- Aeson can derive this automatically as field names "results" and "status" match.
instance FromJSON GeocodingResponse


extractGeoCoordinates :: GeocodingResponse -> (Double, Double)
extractGeoCoordinates GeocodingResponse {..} = undefined

--