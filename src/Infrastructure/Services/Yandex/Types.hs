{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Infrastructure.Services.Yandex.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)
import qualified Data.Vector as V


import Text (camelToSnake, recordLabelModifier)
import Infrastructure.Services.Yandex.Geo (GeoPoint)
import Infrastructure.Services.Yandex.Types.Enums hiding (PickupPoint)
import qualified Infrastructure.Services.Yandex.Types.Enums as Enums
import Infrastructure.Services.Yandex.Order
import Infrastructure.Services.Yandex.Shipment
import Infrastructure.Services.Yandex.Warehouse



type YandexRequestId = Text

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
     { pprGeoId                      :: Maybe GeoId -- ^ The geographic ID for which to list pickup points (e.g., 213 for Moscow)
     , pprPaymentMethods             :: [PaymentMethod] -- ^ Filter pickup points by supported payment method
     , pprType                       :: PickupPointType -- ^ Filter pickup points by type (Terminal, Warehouse, PickupPoint)
     , pprLatitude                   :: Maybe CoordinateInterval -- ^ Optional latitude interval for filtering pickup points
     , pprLongitude                  :: Maybe CoordinateInterval -- ^ Optional longitude interval for filtering pickup points
     , pprIsNotBrandedPartnerStation :: Bool
     } deriving (Show, Eq, Generic)

defaultPickupPointsReq :: PickupPointsReq
defaultPickupPointsReq = 
  PickupPointsReq
  { pprGeoId                      = Just 213 -- Placeholder, should be set to a valid GeoId when making the request
  , pprPaymentMethods             = [PostPay, CardOnReceipt] -- Default to AlreadyPaid and PostPay, can be changed as needed
  , pprType                       = Enums.PickupPoint -- Default to PickupPoint, can be changed to Terminal or Warehouse as needed
  , pprLatitude                   = Nothing
  , pprLongitude                  = Nothing
  , pprIsNotBrandedPartnerStation = True
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
  { info           :: RequestInfo
  , source         :: SourceRequestNode
  , destination    :: DestinationRequestNode
  , billingInfo    :: BillingInfo
  , items          :: [Item]
  , places         :: [Place]
  , recipientInfo  :: RecipientInfo
  , lastMilePolicy :: Tariff
  } deriving (Show, Eq, Generic)

instance ToJSON YandexCreateOrderReq where toJSON = genericToJSON jsonOptions
instance FromJSON YandexCreateOrderReq where parseJSON = genericParseJSON jsonOptions


data PlatformStationId =  PlatformStationId { platformStationId :: Text } deriving (Show, Generic)

instance ToJSON PlatformStationId where toJSON = genericToJSON jsonOptions
instance FromJSON PlatformStationId where parseJSON = genericParseJSON jsonOptions


data PlacePhysicalDimensions =  PlacePhysicalDimensions { physicalDims :: PhysicalDimensions } deriving (Show, Generic)

instance ToJSON PlacePhysicalDimensions where toJSON = genericToJSON jsonOptions
instance FromJSON PlacePhysicalDimensions where parseJSON = genericParseJSON jsonOptions

data PriceCalculatorReq =
     PriceCalculatorReq
     { pcrTariff             :: Tariff
     , pcrDestination        :: PlatformStationId
     , pcrSource             :: PlatformStationId
     , pcrTotalWeight        :: Int32
     , pcrPlaces             :: [PlacePhysicalDimensions]
     , pcrTotalAssessedPrice :: Int32
     } deriving (Show)

data PriceCalculatorResp = 
      PriceCalculatorResp
      { pcrDeliveryDays :: Int32
      , pcrPricingTotal :: Text
      } deriving (Show)


data YandexCreateOrderResp = YandexCreateOrderResp { requestId :: Text } deriving (Show, Eq, Generic)

-- instance ToJSON YandexCreateOrderResp where toJSON = genericToJSON jsonOptions
instance FromJSON YandexCreateOrderResp where parseJSON = genericParseJSON jsonOptions


data YandexParcelLabelReq = YandexParcelLabelReq { requestIds :: [Text] } deriving (Show, Eq, Generic)

instance ToJSON YandexParcelLabelReq where toJSON = genericToJSON jsonOptions
-- instance FromJSON YandexParcelLabelReq where parseJSON = genericParseJSON jsonOptions


data TrackingUrl = TrackingUrl { sharingUrl :: Text } deriving (Show, Eq, Generic)

-- instance ToJSON TrackingUrl where toJSON = genericToJSON jsonOptions
instance FromJSON TrackingUrl where parseJSON = genericParseJSON jsonOptions

data OrderStatus =
     OrderStatus
     { osStatus      :: YandexOrderStatus
     , osDescription :: Text
     , osTimestamp   :: Text
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "os" } ''OrderStatus)

data OrderParticulars = OrderParticulars { state :: OrderStatus } deriving (Show, Eq, Generic)

-- instance ToJSON OrderParticulars where toJSON = genericToJSON jsonOptions
instance FromJSON OrderParticulars where parseJSON = genericParseJSON jsonOptions


data WarehouseCreateReq = WarehouseCreateReq
  { clientWarehouseId :: Text             -- ^ Your internal ID for this warehouse
  , contact           :: WarehouseContact
  , location          :: WarehouseLocation
  , name              :: Text             -- ^ Name displayed in Yandex dashboard
  , merchantId        :: Maybe Text       -- ^ Optional Yandex Merchant ID
  } deriving (Show, Eq, Generic)

instance ToJSON WarehouseCreateReq where toJSON = genericToJSON (jsonOptions { omitNothingFields = True })

data WarehouseCreateResp = WarehouseCreateResp { stationId :: Text }
  deriving (Show, Eq, Generic)

instance FromJSON WarehouseCreateResp where parseJSON = genericParseJSON jsonOptions 


data ManifestReq = ManifestReq { requestIds :: [Text] } deriving (Show, Eq, Generic)

instance ToJSON ManifestReq where  toJSON = genericToJSON jsonOptions

data CreateShipmentReq =
     CreateShipmentReq 
     { parameters              :: PickupParameters
     , pickupLocalDate         :: Text               -- ^ Date format: "YYYY-MM-DD"
     , pickupLocalTimeInterval :: LocalTimeInterval
     , stationId               :: Text               -- ^ UUID for the warehouse station
     } deriving (Show, Eq, Generic)

instance ToJSON CreateShipmentReq where toJSON = genericToJSON jsonOptions 

data CreateShipmentResp = CreateShipmentResp { pickupId :: Text } deriving (Show, Eq, Generic)

instance FromJSON CreateShipmentResp where parseJSON = genericParseJSON jsonOptions 


data PickupOptionsReq = PickupOptionsReq { stationId :: Text } deriving (Show, Eq, Generic)

instance ToJSON PickupOptionsReq where  toJSON = genericToJSON jsonOptions

data PickupOptionsRespItem = 
     PickupOptionsRespItem
     { pickupLocalDate :: Day
     , pickupLocalTimeIntervals :: [LocalTimeInterval]
     } deriving (Show, Eq, Generic)

instance FromJSON PickupOptionsRespItem where parseJSON = genericParseJSON jsonOptions 


data PickupOptionsResp = PickupOptionsResp { pickupOptions :: [PickupOptionsRespItem] } deriving (Show, Eq, Generic)

instance FromJSON PickupOptionsResp where parseJSON = genericParseJSON jsonOptions 


data PickupStatusReq = PickupStatusReq { pickupId :: Text } deriving (Show, Eq, Generic)

instance ToJSON PickupStatusReq where  toJSON = genericToJSON jsonOptions

data PickupStatusRespItem = PickupStatusRespItem { pickupStatus :: PickupStatus} deriving (Show, Eq, Generic)

instance FromJSON PickupStatusRespItem where parseJSON = genericParseJSON jsonOptions 

data PickupStatusResp = PickupStatusResp { pickup :: PickupStatusRespItem } deriving (Show, Eq, Generic)

instance FromJSON PickupStatusResp where parseJSON = genericParseJSON jsonOptions 

data PickupPointAddressReq = PickupPointAddressReq { pickupPointIds :: [Text] } deriving (Show, Eq, Generic)

instance ToJSON PickupPointAddressReq where  toJSON = genericToJSON jsonOptions

newtype PickupPointAddressResp = PickupPointAddressResp Text 

instance FromJSON PickupPointAddressResp where
  parseJSON = withArray "PickupPointAddressResp" $ \vec -> 
    if V.null vec
      then fail "Empty results vector"
      else 
        -- Process ONLY the first element of the vector
        let firstItem = V.head vec
        in flip (withObject "PickupPointAddressResp:PickupPoint") firstItem $ \obj -> do
             addrObj <- obj .: "address"
             PickupPointAddressResp <$> addrObj .: "full_address"


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pcr" } ''PriceCalculatorReq)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pcr" } ''PriceCalculatorResp)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ci" } ''CoordinateInterval)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ppr", omitNothingFields = True } ''PickupPointsReq)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pp" } ''PickupPoint)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ppr" } ''PickupPointsResp)