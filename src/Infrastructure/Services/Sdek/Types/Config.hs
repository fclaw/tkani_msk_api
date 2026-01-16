{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Sdek.Types.Config where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Data.Aeson.Types (camelTo2, defaultOptions, genericParseJSON, parseJSON, fieldLabelModifier)
import Text.Read (readMaybe)


-- | Represents the 'pickup_window' object in YAML
data SdekPickupWindow = SdekPickupWindow
  { from :: Text -- e.g., "14:00"
  , to   :: Text -- e.g., "18:00"
  } deriving (Show, Generic)

instance FromJSON SdekPickupWindow



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

data Sender =
     Sender
     { name   :: Text
     , phone :: Text
     } deriving (Show, Generic)

instance FromJSON Sender where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- | The main SdekConfig record
data SdekConfig = SdekConfig
  { url             :: Text
  , credentials     :: SdekCredentials
  , senderLocation  :: SdekSenderLocation
  , tariffs         :: [Int]
  , pickupWindow    :: SdekPickupWindow
  , accountNumber   :: Text
  , sender          :: Sender
  , pickupMinimum   :: Int
  , dropOffPoint    :: Text
  , commissionRate  :: Double
  } deriving (Show, Generic)

instance FromJSON SdekConfig where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- | Parses the hour from a "HH:MM" string.
--   Returns 'Nothing' if the format is incorrect or the hour is not a valid number.
parseHour :: Text -> Maybe Int
parseHour timeStr =
  -- 1. Split the string by the ":" delimiter
  case T.splitOn ":" timeStr of
    -- 2. Pattern match: We expect exactly two parts ("HH" and "MM")
    [hourPart, _] ->
      -- 3. Try to read the hour part as an Int
      readMaybe (T.unpack hourPart)
      
    -- If the pattern doesn't match (e.g., wrong format), return Nothing
    _ -> Nothing