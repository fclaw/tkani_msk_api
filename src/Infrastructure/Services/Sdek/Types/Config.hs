{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Services.Sdek.Types.Config where


import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Data.Aeson.Types (camelTo2, defaultOptions, genericParseJSON, parseJSON, fieldLabelModifier)

-- | Represents the 'pickup_window' object in YAML
data SdekPickupWindow = SdekPickupWindow
  { from :: Text -- e.g., "14:00"
  , to   :: Text -- e.g., "18:00"
  } deriving (Show, Generic)

instance FromJSON SdekPickupWindow

-- | Represents the 'tariffs' object
data SdekTariffs = SdekTariffs
  { doorToWarehouse :: Int -- Maps to door_to_warehouse
  } deriving (Show, Generic)

instance FromJSON SdekTariffs where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- | Represents the 'sender_location' object
data SdekSenderLocation = SdekSenderLocation
  { address   :: Text
  , cityCode  :: Int
  , postalCode :: Text
  } deriving (Show, Generic)

instance FromJSON SdekSenderLocation where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- | Represents the 'credentials' object
data SdekCredentials = SdekCredentials
  { clientId     :: Text
  , clientSecret :: Text
  } deriving (Show, Generic)

instance FromJSON SdekCredentials where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }


-- | The main SdekConfig record
data SdekConfig = SdekConfig
  { url             :: Text
  , credentials     :: SdekCredentials
  , senderLocation  :: SdekSenderLocation
  , tariffs         :: SdekTariffs
  , pickupWindow    :: SdekPickupWindow
  , accountNumber   :: Text
  } deriving (Show, Generic)

instance FromJSON SdekConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }