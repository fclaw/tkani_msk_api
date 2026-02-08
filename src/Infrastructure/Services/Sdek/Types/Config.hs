{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Sdek.Types.Config where


import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (FromJSON)
import Data.Function (on)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Aeson (withScientific, FromJSON (..))
import Data.Aeson.Types (camelTo2, defaultOptions, genericParseJSON, parseJSON, fieldLabelModifier)


-- | Represents the 'pickup_window' object in YAML
data SdekPickupWindow = SdekPickupWindow
  { from :: Text -- e.g., "14:00"
  , to   :: Text -- e.g., "18:00"
  } deriving (Show, Generic)

instance FromJSON SdekPickupWindow


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

data Tariff = 
      -- | Budget delivery service for companies engaged in remote sales.
       Tariff136 
       -- | Budget ground delivery service for companies engaged in remote sales, Economy variant of Tariff136.
     | Tariff234 
  deriving (Generic, Eq)

-- This helper function assigns a rank to each tariff.
-- The lower the rank, the "smaller" it is in the sort order.
tariffRank :: Tariff -> Int
tariffRank tariff =
  case tariff of
    -- We want Tariff234 to come first, so it gets the lowest rank.
    Tariff234 -> 1
    -- Tariff136 comes second.
    Tariff136 -> 2

-- This is the complete and robust Ord instance.
instance Ord Tariff where
  -- The 'compare' function is the core of the Ord typeclass.
  -- We use the 'on' function to make this clean.
  compare = compare `on` tariffRank
  -- This is equivalent to writing:
  -- compare t1 t2 = compare (tariffRank t1) (tariffRank t2)


instance FromJSON Tariff where
  parseJSON = withScientific "Tariff" $ \s ->
    case round(s) of
      136 -> pure Tariff136
      234 -> pure Tariff234
      t   -> error $ "Unknown tariff: " <> show t

instance Show Tariff where
  show Tariff136 = "Warehouse-to-warehouse (W-W)"
  show Tariff234 = "Economy Warehouse-to-warehouse (W-W)"

tariffToInt :: Tariff -> Int
tariffToInt Tariff136 = 136
tariffToInt Tariff234 = 234

-- | The main SdekConfig record
data SdekConfig = 
     SdekConfig
     { url               :: Text
     , credentials       :: SdekCredentials
     , tariffs           :: [Tariff]
     , pickupWindow      :: SdekPickupWindow
     , sender            :: Sender
     , pickupMinimum     :: Int
     , dropOffPoint      :: Text
     , commissionRate    :: Double
     , consolidationTime :: Int
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