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

-- SdekLocation needs to be used for both from and to.
-- It now primarily uses the address, not the city code.
data SdekLocation = SdekLocation
  { locAddress :: Text -- The full address of the sender/receiver PVZ.
  } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "loc" } ''SdekLocation)

-- This is the main record you will construct. It contains only the fields
-- SDEK requires to get an order into their system for later manual editing.
data SdekOrderRequest = SdekOrderRequest
  { sorTariffCode :: Int            -- REQUIRED: e.g., 137 for "Склад-ПВЗ".
  , sorRecipient  :: SdekRecipient  -- REQUIRED: Minimal recipient info.
  , sorPackages   :: [SdekPackage]  -- REQUIRED: Minimal package info.
  , sorShipmentPoint :: Text           -- REQUIRED: The code of the PVZ you are shipping FROM.
  , sorDeliveryPoint :: Text           -- REQUIRED: The code of the PVZ the customer chose.
  } deriving (Show, Generic)

-- This instance converts Haskell's camelCase to SDEK's snake_case.
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sor" } ''SdekOrderRequest)

-- | ================================================================
-- | The Asynchronous Request State ENUM
-- | ================================================================

-- | Represents the state of an asynchronous request, as returned by SDEK.
data SdekRequestState
  = Accepted     -- "ACCEPTED":   Request is valid and queued for processing.
  | Waiting      -- "WAITING":    Request is waiting for another request to complete.
  | Successful   -- "SUCCESSFUL": The entity (e.g., order) was successfully created.
  | Invalid      -- "INVALID":    The request failed deep validation.
  | UnknownState Text -- A catch-all for any future statuses SDEK might add.
  deriving (Show, Eq, Generic)

-- To parse this enum from SDEK's JSON strings, we need a custom FromJSON instance.
instance FromJSON SdekRequestState where
  parseJSON = withText "SdekRequestState" $ \t ->
    pure $ case t of
      "ACCEPTED"   -> Accepted
      "WAITING"    -> Waiting
      "SUCCESSFUL" -> Successful
      "INVALID"    -> Invalid
      -- This catch-all makes our parsing robust against API changes.
      other        -> UnknownState other

-- to meet the derivation of SdekConfirmation
instance ToJSON SdekRequestState where
  toJSON _ = undefined

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

data SdekConfirmation = SdekConfirmation { scStatus :: SdekRequestState }
  deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sc" } ''SdekConfirmation)