{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}

module Infrastructure.Services.Yandex.Types.Enums where

import Data.Aeson.TH
import Data.Text (unpack)
import Data.Aeson (defaultOptions, SumEncoding(..), FromJSON (..), ToJSON (..), withText, genericParseJSON, genericToJSON)
import GHC.Generics (Generic)
import Data.Char (toLower, toUpper)

import Text (camelToSnake)


data PickupPointType = Terminal | Warehouse | PickupPoint deriving (Show, Eq)

data PaymentMethod = AlreadyPaid | PostPay | CardOnReceipt | BoundCard deriving (Show, Eq)

instance FromJSON PaymentMethod where
  parseJSON = withText "PaymentMethod" $ \case
    "already_paid"     -> pure AlreadyPaid
    "card_on_receipt"  -> pure CardOnReceipt -- Yandex sometimes alternates these
    "postpay"          -> pure PostPay
    "bound_card"       -> pure BoundCard
    method             -> fail $ "Unknown PaymentMethod: " <> unpack method

instance ToJSON PaymentMethod where
  toJSON method = case method of
    AlreadyPaid     -> "already_paid"
    CardOnReceipt   -> "card_on_receipt"
    PostPay         -> "postpay"
    BoundCard       -> "bound_card"

instance FromJSON PickupPointType where
  parseJSON = withText "PickupPointType" $ \case
    "terminal"      -> pure Terminal
    "warehouse"     -> pure Warehouse
    "pickup_point"  -> pure PickupPoint
    pType           -> fail $ "Unknown PickupPointType: " <> unpack pType

instance ToJSON PickupPointType where
  toJSON pType = case pType of
    Terminal    -> "terminal"
    Warehouse   -> "warehouse"
    PickupPoint -> "pickup_point"

data NodeType = PlatformStation | CustomLocation deriving (Show, Eq)

instance FromJSON NodeType where
  parseJSON = withText "NodeType" $ \case
    "platform_station" -> pure PlatformStation
    "custom_location"  -> pure CustomLocation
    nodeType           -> fail $ "Unknown NodeType: " <> unpack nodeType

instance ToJSON NodeType where
  toJSON nodeType = case nodeType of
    PlatformStation -> "platform_station"
    CustomLocation  -> "custom_location"

data Tariff = SelfPickup | TimeInterval deriving (Show, Eq, Generic)

instance FromJSON Tariff where
  parseJSON = withText "Tariff" $ \case
    "time_interval" -> pure TimeInterval
    "self_pickup"   -> pure SelfPickup
    tariff          -> fail $ "Unknown Tariff: " <> unpack tariff

instance ToJSON Tariff where
  toJSON tariff = case tariff of
    SelfPickup -> "self_pickup"
    TimeInterval  -> "time_interval"



-- | Statuses of a Yandex Delivery Request (Claim)
data YandexOrderStatus
  = Draft                          -- ^ Order draft created
  | Validating                     -- ^ Request is being validated/checked
  | ValidatingError                -- ^ Order not confirmed by sorting center (error)
  | Created                        -- ^ Order created and confirmed
  | DeliveryProcessingStarted      -- ^ Order being created at sorting center
  | DeliveryTrackReceived          -- ^ Order created in the carrier's system (API typo: RECIEVED)
  | SortingCenterProcessingStarted -- ^ Processing started at sorting center (SC)
  | SortingCenterTrackReceived     -- ^ Order processed at sorting center
  | SortingCenterTrackLoaded       -- ^ Order record created at sorting center
  | DeliveryLoaded                 -- ^ Order added to current shipment (batch)
  | SortingCenterLoaded            -- ^ Order confirmed at sorting center
  | SortingCenterAtStart           -- ^ Order arrived at sorting center
  | SortingCenterPrepared          -- ^ Order ready for dispatch to carrier
  | SortingCenterTransmitted       -- ^ Order is currently being delivered
  | DeliveryAtStart                -- ^ Order being prepared for final delivery
  | DeliveryTransportation         -- ^ Order departed for destination (in transit)
  | DeliveryArrivedPickupPoint     -- ^ Order arrived at pickup point (PVZ)
  | DeliveryTransmittedToRecipient -- ^ Order handed over to recipient
  | DeliveryStoragePeriodExpired   -- ^ Storage period at pickup point expired
  | DeliveryStoragePeriodExtended  -- ^ Storage period at pickup point extended
  | ConfirmationCodeReceived       -- ^ Confirmation code received
  | ParticularlyDelivered          -- ^ Order partially delivered
  | DeliveryDelivered              -- ^ Order delivered to recipient (final)
  | Finished                       -- ^ Order confirmed/closed
  deriving (Show, Eq, Generic)

-- =============================================================================
-- JSON Configuration
-- =============================================================================

yandexStatusOptions :: Options
yandexStatusOptions = defaultOptions
  { constructorTagModifier = \case
      -- Fix for the typo in Yandex API: they send "RECIEVED" not "RECEIVED"
      "DeliveryTrackReceived" -> "DELIVERY_TRACK_RECIEVED"
      -- General rule for others: CamelCase -> UPPER_SNAKE_CASE
      other -> map toUpper (camelToSnake other)
  }

instance ToJSON YandexOrderStatus where
  toJSON = genericToJSON yandexStatusOptions

instance FromJSON YandexOrderStatus where
  parseJSON = genericParseJSON yandexStatusOptions


data PickupStatus = Scheduled | Completed | Cancelled deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''PickupStatus)