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


-- | Statuses of a Yandex Delivery Request (Claim) based on current documentation.
data YandexOrderStatus
  = Draft                          -- ^ Order created
  | Validating                     -- ^ Request is under verification
  | ValidatingError                -- ^ Order not confirmed at the sorting center
  | Created                        -- ^ Order created and confirmed
  | DeliveryProcessingStarted      -- ^ Order is being created at the sorting center
  | DeliveryTrackReceived          -- ^ Order created in the delivery service system (API typo: RECIEVED)
  | SortingCenterProcessingStarted -- ^ Order processing started at the sorting center
  | SortingCenterTrackReceived     -- ^ Order processed at the sorting center
  | SortingCenterTrackLoaded       -- ^ Order created at the sorting center
  | DeliveryLoaded                 -- ^ Order added to the current shipment
  | SortingCenterLoaded            -- ^ Order confirmed at the sorting center
  | SortingCenterAtStart           -- ^ Order arrived at the acceptance point
  | SortingCenterPrepared          -- ^ Order ready for dispatch to the delivery service
  | SortingCenterTransmitted       -- ^ Order is being delivered to the last mile
  | DeliveryAtStart                -- ^ Order is in the recipient's city, preparing for courier delivery
  | DeliveryAtStartSort            -- ^ Order is in the recipient's city, preparing for courier delivery (sorting stage)
  | DeliveryTransportationRecipient -- ^ Order is being delivered to the customer
  | DeliveryTransmittedToRecipient -- ^ Order handed over to the recipient
  | DeliveryAttemptFailed          -- ^ Unsuccessful delivery attempt
  | DeliveryDelivered              -- ^ Order delivered to the customer
  deriving (Show, Eq, Generic)

-- =============================================================================
-- JSON Configuration (Mapping constructor names to Yandex Upper Snake Case)
-- =============================================================================

yandexStatusOptions :: Options
yandexStatusOptions = defaultOptions
  { constructorTagModifier = \case
      -- The API preserves this specific spelling mistake
      "DeliveryTrackReceived" -> "DELIVERY_TRACK_RECIEVED"
      -- Common CamelCase -> UPPER_SNAKE_CASE conversion
      other -> map toUpper (camelToSnake other)
  }

instance ToJSON YandexOrderStatus where
  toJSON = genericToJSON yandexStatusOptions

instance FromJSON YandexOrderStatus where
  parseJSON = genericParseJSON yandexStatusOptions




data PickupStatus = Scheduled | Completed | Cancelled deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''PickupStatus)