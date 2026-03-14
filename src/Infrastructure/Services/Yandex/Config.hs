{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Infrastructure.Services.Yandex.Config where

import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Infrastructure.Services.Yandex.Geo
import Data.Aeson.Types (camelTo2)
import Data.Aeson (FromJSON (..), genericParseJSON, defaultOptions, fieldLabelModifier)





data Address =
     Address
     { city    :: Text
     , house   :: Text
     , building :: Text
     , floor    :: Text
     , street   :: Text
     } deriving (Show, Eq, Generic)

instance FromJSON Address where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data Contact =
     Contact
     { name    :: Text
     , surname :: Text
     , phone   :: Text
     , email   :: Text
     } deriving (Show, Eq, Generic)

instance FromJSON Contact where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data PickupWindow = PickupWindow { fromHour :: Text, toHour :: Text } deriving (Show, Eq, Generic)

instance FromJSON PickupWindow where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data YandexConfig =
     YandexConfig
     { apiKey            :: Text
     , apiUrl            :: Text
     , office            :: GeoPoint
     , localWarehouseId  :: Text
     , warehouseName     :: Text
     , address           :: Address
     , contact           :: Contact
     , warehousePostfix  :: Text
     , pickupParcels     :: Int
     , pickupWeight      :: Int
     , pickupWindow      :: PickupWindow
     } deriving (Show, Eq, Generic)

instance FromJSON YandexConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }