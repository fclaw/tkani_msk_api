{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DataKinds  #-}

module Infrastructure.Services.Sdek.Types where


import Data.Aeson
import Data.Aeson.TH
import Text (recordLabelModifier)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Text (Text)
import Data.UUID (UUID, fromText)
import qualified Data.Vector as V

import Text (camelToSnake)
import Infrastructure.Services.Sdek.Types.State (SdekRequestState)


-- | Internal data type to decode the city search response from SDEK.
newtype SdekCity = SdekCity { code :: Int }
  deriving (Show)

-- | Manual FromJSON instance to extract the "code" field from the JSON object.
instance FromJSON SdekCity where
  parseJSON = withObject "SdekCity" $ \v -> do
    -- 'v' is the JSON object.
    -- The '.:' operator looks up the "code" key and parses its value as an Int.
    cityCode <- v .: "code"
    -- We then wrap the resulting Int in our SdekCity constructor.
    pure $ SdekCity cityCode


-- | ================================================================
-- | The MINIMAL Top-Level Order Request (`/v2/orders` payload)
-- | ================================================================

-- | ================================================================
-- | Minimal Sub-records for the Order Request
-- | ================================================================


-- | A simple phone number record, as SDEK expects a an array of objects.
data SdekPhone = SdekPhone { phNumber :: Text } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ph" } ''SdekPhone)


-- | Minimal information about the recipient.
--   'name' and at least one phone number are usually the true minimum.
data SdekRecipient = SdekRecipient
  { rcpName   :: Text
  , rcpPhones :: [SdekPhone]
  } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "rcp" } ''SdekRecipient)

-- SDEK requires a 'payment' object for the item cost.
data SdekPayment = SdekPayment { payValue :: Double } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pay" } ''SdekPayment)

-- | An item inside a package. We can create a sensible default.
data SdekPackageItem = SdekPackageItem
  { pkiName     :: Text  -- Name, e.g., "Ткань"
  , pkiWareKey  :: Text  -- Your internal SKU. Can be the fabric ID.
  , pkiPayment  :: SdekPayment -- Payment details, usually "cost".
  , pkiWeight   :: Int   -- Weight in grams.
  , pkiAmount   :: Int   -- Quantity.
  , pkiCost     :: Int
  } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pki" } ''SdekPackageItem)

-- | Minimal package/parcel information.
--   Weight is almost always required.
data SdekPackage = SdekPackage
  { pkgNumber :: Text -- Your internal identifier, can be "1".
  , pkgWeight :: Int  -- Weight in grams. This is critical.
  , pkgItems  :: [SdekPackageItem] -- REQUIRED
  } deriving (Show, Generic)


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pkg" } ''SdekPackage)

-- | ================================================================
-- | The 'services' array for SDEK Order and Calculator requests
-- | ================================================================

-- | A sum type (enum) for known SDEK service codes for better type safety.
data SdekServiceCode
  = INSURANCE    -- Страхование
  | NONE
  -- Add other service codes here as you need them
  deriving (Show, Generic)

$(deriveJSON defaultOptions { sumEncoding = UntaggedValue } ''SdekServiceCode)

-- | Represents a single service object. The 'code' determines the meaning of 'parameter'.
--   We define separate "smart constructors" for clarity instead of using the raw type.
data SdekService = SdekService
  { ssCode      :: SdekServiceCode -- Use a safe enum type for the service code
  , ssParameter :: Maybe Text      -- Parameter is a string in some cases, a number in others. We'll handle this.
  } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ss" } ''SdekService)

-- | Represents the sender's address for a "from the door" tariff.
--   The fields here are based on the standard SDEK API structure.
data SdekFromLocation = SdekFromLocation
  { sflAddress   :: Text -- Full street address, e.g., "ул. Ленина, д. 1, кв. 2"
  , sflCode      :: Int  -- SDEK's internal numeric code for the city
  , sflPostCode  :: Maybe Text -- Postal code, often required
  } deriving (Show, Eq, Generic)

-- Generate snake_case JSON instance (e.g., sflCityCode -> city_code)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sfl"} ''SdekFromLocation)

defSdekFromLocation :: SdekFromLocation
defSdekFromLocation = SdekFromLocation mempty 0 Nothing


-- This is the main record you will construct. It contains only the fields
-- SDEK requires to get an order into their system for later manual editing.
data SdekOrderRequest = SdekOrderRequest
  { sorTariffCode    :: Int            -- REQUIRED: e.g., 137 for "Склад-ПВЗ".
  , sorRecipient     :: SdekRecipient  -- REQUIRED: Minimal recipient info.
  , sorPackages      :: [SdekPackage]  -- REQUIRED: Minimal package info.
  , sorFromLocation  :: Maybe SdekFromLocation  -- REQUIRED: Location point if tariff 232
  , sorShipmentPoint :: Maybe Text  -- REQUIRED: Location point if tariff 234, cannot be used simultaneously with 232
  , sorDeliveryPoint :: Text           -- REQUIRED: The code of the PVZ the customer chose.
  , sorServices      :: [SdekService]
  } deriving (Show, Generic)

-- This instance converts Haskell's camelCase to SDEK's snake_case.
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sor", omitNothingFields = True } ''SdekOrderRequest)


-- | ================================================================
-- | The Order Creation RESPONSE
-- | ================================================================

-- | Represents the immediate response from the POST /v2/orders endpoint.
--   It contains the UUID for tracking and any initial validation errors.
data SdekOrderResponse = SdekOrderResponse
  { sorEntityUuid   :: UUID -- The main 'entity.uuid', our 'request_uuid'.
  , sorRequestState :: SdekRequestState -- The 'requests[0].state' (e.g., "ACCEPTED", "INVALID").
  , sorErrors       :: Maybe [SdekError]  -- Any immediate 'requests[0].errors'.
  } deriving (Show, Generic)

-- To handle the complex nested structure, we need a custom FromJSON instance.
instance FromJSON SdekOrderResponse where
  parseJSON = withObject "SdekOrderResponse" $ \o -> do
    -- 1. Get the 'entity' object and extract its 'uuid'.
    entityObj <- o .: "entity"
    -- THIS IS THE NEW, SAFE LOGIC FOR PARSING THE UUID
    rawUuidText <- entityObj .: "uuid"
    let errorMsg = "Invalid UUID format in 'entity.uuid': " <> T.unpack rawUuidText
    parsedUuid <- 
      case fromText rawUuidText of
        Nothing -> fail $ errorMsg
        Just uuid -> pure uuid

    -- 2. Get the 'requests' array. We assume it always has at least one element.
    requestsArr <- o .: "requests"
    requestObj <- case V.toList requestsArr of
      (firstReq:_) -> pure firstReq
      []           -> fail "Expected at least one 'request' object, but the array was empty."

    -- 3. Extract the 'state' and 'errors' from the first request object.
    withObject "RequestObject" (\req -> do
        reqState <- req .: "state"
        reqErrors <- req .:? "errors"
        pure $ SdekOrderResponse parsedUuid reqState reqErrors
      ) requestObj

-- | Represents a single error object from the 'errors' array.
data SdekError = SdekError
  { seCode    :: Text
  , seMessage :: Text
  } deriving (Show, Generic)

instance FromJSON SdekError where
  parseJSON = withObject "SdekError" $ \o ->
    SdekError <$> o .: "code" <*> o .: "message"


unexpected_response :: Text
unexpected_response = "UNEXPECTED_RESPONSE"

data SdekConfirmation = SdekConfirmation { scStatus :: SdekRequestState }
  deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sc" } ''SdekConfirmation)

-- | ================================================================
-- | The Order Information RESPONSE (`GET /v2/orders/{uuid}`)
-- | This is the data structure our polling worker needs.
-- | ================================================================

-- | The top-level response. We care about the final tracking number and the request statuses.
data SdekOrderStatusResponse = SdekOrderStatusResponse
  { sosrCdekNumber :: Maybe Text           -- The official 'cdek_number', present on SUCCESSFUL
  , sosrRequests   :: [SdekPollingStatus]  -- The array of request statuses
  } deriving (Show, Generic)

instance FromJSON SdekOrderStatusResponse where
  parseJSON = withObject "SdekOrderStatusResponse" $ \o -> do
    entity <- o .: "entity"
    SdekOrderStatusResponse
      <$> entity .:? "cdek_number" -- Optional field
      <*> o      .: "requests"

-- | Represents the status of the original request that we are polling.
data SdekPollingStatus = SdekPollingStatus
  { spsRequestUuid :: UUID
  , spsState       :: SdekRequestState -- Use our existing safe enum!
  , spsErrors      :: [SdekError]
  } deriving (Show, Generic)

instance FromJSON SdekPollingStatus where
  parseJSON = withObject "SdekPollingStatus" $ \o ->
    SdekPollingStatus
      <$> (o .: "request_uuid" >>= parseUuid)
      <*> o .: "state"
      <*> o .:? "errors" .!= []
    where
      errorMsg = ("Invalid UUID format in 'entity.uuid': " <>) . T.unpack
      parseUuid rawUuidText =
        case fromText rawUuidText of
          Nothing -> fail $ errorMsg rawUuidText
          Just uuid -> pure uuid

data SenderPhone = SenderPhone { spNumber :: Text }
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake . drop 2 } ''SenderPhone)

data SdekCallCourierSender = SdekCallCourierSender
  { scsName  :: Text
  , scsPhones :: [SenderPhone]
  } deriving (Show, Eq, Generic)


$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake . drop 3 } ''SdekCallCourierSender)

-- | Request body for the "Call Courier" endpoint.
data SdekCallCourierRequest = SdekCallCourierRequest
  { -- | The UUID of the order for which we are requesting a courier pickup.
    sccrOrderUuid      :: UUID
    -- | The desired date for the courier to arrive (YYYY-MM-DD).
  , sccrIntakeDate     :: Text
    -- | The start of the pickup time window (HH:MM).
  , sccrIntakeTimeFrom :: Text
    -- | The end of the pickup time window (HH:MM).
  , sccrIntakeTimeTo   :: Text
    -- You would also include sender info here if not linked to the order
  , sccrSender         :: SdekCallCourierSender
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake . drop 4 } ''SdekCallCourierRequest)