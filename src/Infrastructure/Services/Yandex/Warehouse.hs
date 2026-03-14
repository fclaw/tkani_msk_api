{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Infrastructure.Services.Yandex.Warehouse where

import           Data.Aeson          (ToJSON (..), Options (..), defaultOptions, genericToJSON)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)



import Text (camelToSnake)
import Infrastructure.Services.Yandex.Geo (GeoPoint)

-- | Shared JSON options for snake_case mapping
yandexOptions :: Options
yandexOptions = defaultOptions { fieldLabelModifier = camelToSnake, omitNothingFields = True }

-- -----------------------------------------------------------------------------
-- WarehouseContact
-- -----------------------------------------------------------------------------

data WarehouseContact = WarehouseContact
  { phone      :: Text        -- ^ Format: "+7..."
  , email      :: Maybe Text
  , firstName  :: Text
  , lastName   :: Text
  , patronymic :: Maybe Text  -- ^ Russian Middle Name
  } deriving (Show, Eq, Generic)

instance ToJSON WarehouseContact where toJSON = genericToJSON yandexOptions

-- -----------------------------------------------------------------------------
-- WarehouseLocation & Address
-- -----------------------------------------------------------------------------

data WarehouseLocation = WarehouseLocation
  { address     :: WarehouseAddress
  , coordinates :: GeoPoint
  } deriving (Show, Eq, Generic)

instance ToJSON WarehouseLocation where toJSON = genericToJSON yandexOptions

data WarehouseAddress = WarehouseAddress
  { city       :: Text
  , country    :: Maybe Text
  , region     :: Maybe Text
  , street     :: Maybe Text
  , house      :: Text
  , building   :: Maybe Text
  , apartment  :: Maybe Text
  , floor      :: Maybe Text
  , entrance   :: Maybe Text
  , doorCode   :: Maybe Text
  , postalCode :: Maybe Text
  , geoId      :: Maybe Int          -- ^ Yandex Settlement ID
  } deriving (Show, Eq, Generic)

instance ToJSON WarehouseAddress where toJSON = genericToJSON yandexOptions

defWarehouseAddress = WarehouseAddress mempty Nothing Nothing Nothing mempty Nothing Nothing Nothing Nothing Nothing Nothing Nothing