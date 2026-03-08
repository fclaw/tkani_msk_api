{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TemplateHaskell       #-}

module Infrastructure.Services.Yandex.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics (Generic)


import Text (camelToSnake, recordLabelModifier)
import Infrastructure.Services.Yandex.Geo (GeoPoint)
import Infrastructure.Services.Yandex.Types.Enums hiding (PickupPoint)
import qualified Infrastructure.Services.Yandex.Types.Enums as Enums
import Infrastructure.Services.Yandex.Order



-- | Shared configuration for the JSON instances to handle snake_case mapping.
jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelToSnake }


-- | Request body for location detection.
data LocationDetectReq = LocationDetectReq
  { location :: Text -- ^ Address or city fragment (e.g., "Москва")
  } deriving (Show, Eq, Generic)

instance ToJSON LocationDetectReq where
  toJSON = genericToJSON jsonOptions

type GeoId = Int

-- | An individual settlement match returned by Yandex.
data LocationDetectedVariant = LocationDetectedVariant
  { geoId   :: GeoId  -- ^ Yandex specific geographic ID (e.g., 213 for Moscow)
  , address :: Text -- ^ Resolved full address string
  } deriving (Show, Eq, Generic)

instance FromJSON LocationDetectedVariant where
  parseJSON = genericParseJSON jsonOptions

-- | The wrapper response containing a list of potential location matches.
data LocationDetectResp = LocationDetectResp
  { -- | A list of detected locations. Each entry corresponds to a potential match for the input query.
    -- These are **ordered by relevance** from the most precise/relevant match 
    -- to the least precise matches or broader fragments.
    variants :: [LocationDetectedVariant]
  } deriving (Show, Eq, Generic)

instance FromJSON LocationDetectResp where
  parseJSON = genericParseJSON jsonOptions


data CoordinateInterval = 
     CoordinateInterval 
     { ciFrom :: Double
     , ciTo   :: Double
     } deriving (Show, Eq, Generic)

data PickupPointsReq = 
     PickupPointsReq
     { pprGeoId           :: Maybe GeoId -- ^ The geographic ID for which to list pickup points (e.g., 213 for Moscow)
     , pprPaymentMethod   :: PaymentMethod -- ^ Filter pickup points by supported payment method
     , pprType            :: PickupPointType -- ^ Filter pickup points by type (Terminal, Warehouse, PickupPoint)
     , pprLatitude        :: Maybe CoordinateInterval -- ^ Optional latitude interval for filtering pickup points
     , pprLongitude       :: Maybe CoordinateInterval -- ^ Optional longitude interval for filtering pickup points
     } deriving (Show, Eq, Generic)

defaultPickupPointsReq :: PickupPointsReq
defaultPickupPointsReq = PickupPointsReq
  { pprGeoId         = Just 213 -- Placeholder, should be set to a valid GeoId when making the request
  , pprPaymentMethod = CardOnReceipt -- Default to AlreadyPaid, can be changed as needed
  , pprType          = Enums.PickupPoint -- Default to PickupPoint, can be changed to Terminal or Warehouse as needed
  , pprLatitude      = Nothing
  , pprLongitude     = Nothing
  }


data Address = Address
     { fullAddress :: Text -- ^ The complete address string (e.g., "ул. Ленина, д. 1, Москва")
     } deriving (Show, Eq, Generic)

instance FromJSON Address where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Address where
  toJSON = genericToJSON jsonOptions

data PickupPoint =
     PickupPoint
     { ppId                :: Maybe Text -- ^ Unique identifier for the pickup point
     , ppName              :: Text -- ^ Name of the pickup point (e.g., "Пункт выдачи №123")
     , ppAddress           :: Address -- ^ Full address of the pickup point
     , ppType              :: PickupPointType -- ^ Type of the pickup point (Terminal, Warehouse, PickupPoint)
     , ppPaymentMethods    :: [PaymentMethod] -- ^ List of payment methods supported at this pickup point
     , ppOperatorStationId :: Maybe Text -- ^ Optional operator station ID if applicable
     , ppPosition          :: GeoPoint -- ^ Optional geographic coordinates of the pickup point
     } deriving (Show, Eq, Generic)

type DropOffPoint = PickupPoint


data PickupPointsResp = 
     PickupPointsResp
     { pprPoints :: [PickupPoint] -- ^ List of pickup points matching the criteria
     } deriving (Show, Eq, Generic)


-- =============================================================================
-- Core Request Payload: POST /api/b2b/platform/request/create
-- =============================================================================

data YandexCreateOrderReq = YandexCreateOrderReq
  { info         :: RequestInfo
  , source       :: LocationNode
  , destination  :: LocationNode
  , billingInfo  :: BillingInfo
  , items        :: [Item]
  , places       :: [Place]
  } deriving (Show, Eq, Generic)

instance ToJSON YandexCreateOrderReq where toJSON = genericToJSON jsonOptions

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ci" } ''CoordinateInterval)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ppr", omitNothingFields = True } ''PickupPointsReq)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pp" } ''PickupPoint)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ppr" } ''PickupPointsResp)