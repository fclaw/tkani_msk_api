{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Infrastructure.Services.Yandex.Config where

import Data.Text (Text)
import GHC.Generics (Generic)
import Infrastructure.Services.Yandex.Geo
import Data.Aeson.Types (camelTo2)
import Data.Aeson (FromJSON (..), genericParseJSON, defaultOptions, fieldLabelModifier)

data YandexConfig = 
     YandexConfig
     { apiKey :: Text
     , apiUrl :: Text
     , office :: GeoPoint
     } deriving (Show, Eq, Generic)

instance FromJSON YandexConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }