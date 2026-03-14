{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Yandex.Shipment where

import           Data.Aeson          (ToJSON (..), FromJSON (..), Options (..), defaultOptions, genericToJSON, genericParseJSON)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

import Text (camelToSnake)

-- | JSON Options to map camelCase field names to snake_case used by Yandex API
yandexOptions :: Options
yandexOptions = defaultOptions { fieldLabelModifier = camelToSnake, omitNothingFields = True }

-- -----------------------------------------------------------------------------
-- PickupParameters
-- -----------------------------------------------------------------------------

data PickupParameters = PickupParameters
  { volumeM3     :: Text                 -- ^ Note: Example shows this as a string "12.50"
  , weightG      :: Int                  -- ^ Total weight in grams
  , requirements :: Maybe PickupRequirements
  } deriving (Show, Eq, Generic)

instance ToJSON PickupParameters where 
  toJSON = genericToJSON yandexOptions

data PickupRequirements = PickupRequirements
  { loadersRequired :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON PickupRequirements where 
  toJSON = genericToJSON yandexOptions

-- -----------------------------------------------------------------------------
-- LocalTimeInterval
-- -----------------------------------------------------------------------------

data LocalTimeInterval = LocalTimeInterval
  { from :: Text -- ^ Format: "HH:MM"
  , to   :: Text -- ^ Format: "HH:MM"
  } deriving (Show, Eq, Generic)

instance ToJSON LocalTimeInterval where 
  toJSON = genericToJSON yandexOptions


instance FromJSON LocalTimeInterval where
  parseJSON = genericParseJSON yandexOptions 
