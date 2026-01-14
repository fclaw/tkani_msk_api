{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Infrastructure.Services.Dostavista.Types.Config where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Data.Aeson.Types (camelTo2, defaultOptions, genericParseJSON, parseJSON, fieldLabelModifier)


data Point =
     Point
     { address   :: Text
     , latitude  :: Text
     , longitude :: Text
     } deriving (Show, Generic, FromJSON)

data Contact = 
     Contact 
     { name    :: Text
     , surname :: Text
     , phone   :: Text
     } deriving (Show, Generic, FromJSON)



-- | The main Dostavista record
data DostavistaConfig = 
     DostavistaConfig
     { courierCallCutoffHour :: Int
     , token                 :: Text
     , url                   :: Text
     , source                :: Point
     , destination           :: Point
     , contact               :: Contact
     } deriving (Show, Generic)

instance FromJSON DostavistaConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }