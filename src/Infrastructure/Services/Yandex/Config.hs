{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Infrastructure.Services.Yandex.Config where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (..), genericParseJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Types (camelTo2)

data YandexConfig = 
     YandexConfig
     { apiKey :: Text
     , apiUrl :: Text
     } deriving (Show, Eq, Generic)

instance FromJSON YandexConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }