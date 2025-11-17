-- file: src/Infrastructure/Services/Yandex/Types.hs

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-} -- For convenient parsing

module Infrastructure.Services.Yandex.Types
  ( YandexResponse(..)
  , YandexGeoObject(..)
  , extractMetroNames
  )
where

import           Data.Aeson
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- We will define the types from the top of the JSON down.

-- | The top-level response object.
data YandexResponse = YandexResponse
  { yrGeoObjectCollection :: GeoObjectCollection
  } deriving (Show, Generic)

-- To get the nested 'response.GeoObjectCollection' field.
instance FromJSON YandexResponse where
  parseJSON = withObject "YandexResponse" $ \o -> do
    responseObj <- o .: "response"
    YandexResponse <$> responseObj .: "GeoObjectCollection"


-- | The collection that holds the list of found geographical objects.
data GeoObjectCollection = GeoObjectCollection
  { gocFeatureMember :: Vector FeatureMember -- Yandex returns an array
  } deriving (Show, Generic)

instance FromJSON GeoObjectCollection where
  parseJSON = withObject "GeoObjectCollection" $ \o ->
    GeoObjectCollection <$> o .: "featureMember"


-- | Each member in the 'featureMember' array contains a GeoObject.
data FeatureMember = FeatureMember
  { fmGeoObject :: YandexGeoObject
  } deriving (Show, Generic)

instance FromJSON FeatureMember where
  parseJSON = withObject "FeatureMember" $ \o ->
    FeatureMember <$> o .: "GeoObject"


-- | This is the core data we care about: the GeoObject itself.
--   It contains the name of the metro station.
data YandexGeoObject = YandexGeoObject
  { ygoName :: Text
  -- You could add other fields here if needed, like 'description' or 'Point'.
  } deriving (Show, Generic)

instance FromJSON YandexGeoObject where
  parseJSON = withObject "YandexGeoObject" $ \o ->
    YandexGeoObject <$> o .: "name"


--
-- -- A HELPER FUNCTION TO EXTRACT THE DATA --
--

-- | A pure function that traverses the nested YandexResponse structure
--   to extract a clean list of metro station names.
extractMetroNames :: YandexResponse -> [Text]
extractMetroNames response =
  -- Use Vector's 'toList' and 'fmap' (or list comprehensions) for a clean extraction.
  let featureMembers = V.toList . gocFeatureMember . yrGeoObjectCollection $ response
      geoObjects     = map fmGeoObject featureMembers
      metroNames     = map ygoName geoObjects
  in metroNames