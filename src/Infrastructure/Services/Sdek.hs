{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE DataKinds  #-}


module Infrastructure.Services.Sdek 
       ( SdekError
       , getDeliveryPoints
       , registerOrder 
       , buildMinimalOderRequest
       ) where

import Data.Text (Text)
import Control.Monad.IO.Class
import Katip
import Data.Aeson
import Data.Aeson.TH
import Text (recordLabelModifier)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Control.Monad (forM_, void, when)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Concurrent.STM (atomically, readTVar)
import Data.Time (UTCTime, diffUTCTime)
import qualified Data.HashMap.Strict as HM
import Data.UUID (UUID, fromText)
import qualified Data.Vector as V

import App (AppM, sdekAccessToken, _sdekUrl, _pointCache, currentTime)
import API.Types
import Infrastructure.Utils.Http
import Infrastructure.Services.Sdek.Auth (getValidSdekToken)
import TH.Location (currentModule)
import API.WithField (WithField (..))
import Infrastructure.Services.Sdek.CachedDeliveryPoints (storeDeliveryPoints)


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

getDeliveryPoints :: Text -> AppM (ApiResponse [WithField "dpMetros" [T.Text] DeliveryPoint])
getDeliveryPoints city = do
  url <- fmap (T.unpack . _sdekUrl) ask
  -- Step 1: Find the SDEK city code.
  $(logTM) InfoS $ logStr $ "Fetching SDEK city code for " <> city
  let cityUrl = "https://" <> url <> "/v2/location/cities"
  let cityParams =
        [ ("country_codes", "RU")  -- THE FIX: Limit search to Russia
        , ("city", city)           -- The city name to search for
        , ("size", "1")            -- Optional but good practice: we only need one result
        , ("lang", "rus")          -- Optional but good practice: ensure Russian response
        ]
  let cityReq = getValidSdekToken >>= (_getReq' cityUrl cityParams . Just . sdekAccessToken)
  eCities <- makeRequestWithRetries @[SdekCity] (Just (void $ getValidSdekToken)) cityReq
  handleApiResponse @_ @[SdekCity] $(currentModule) eCities $ \case
    [] -> do
      $(logTM) InfoS $ logStr $ "SDEK city not found for: " <> city
      pure $ Right [] -- Return an empty list, which is a valid success case.
    (_:_:_) -> do 
      $(logTM) InfoS $ logStr $ "SDEK city not found (no exact match): " <> city
      pure $ Right []
    (firstCity:_) -> do
        stateTVar <- get
        now <- currentTime
        -- Read the current cache content atomically
        currentCache <- fmap _pointCache $ liftIO $ atomically $ readTVar stateTVar
        -- Check if a valid, fresh entry exists
        case HM.lookup (code firstCity) currentCache of
          Just (timestamp, cachedPoints) | isFresh now timestamp -> do
            -- CACHE HIT
            $(logTM) InfoS $ "Cache hit for city: " <> ls city
            return $ Right cachedPoints
          _ -> do
            $(logTM) InfoS $ "Cache miss for city: " <> ls city <> ". Fetching from APIs..."
            storeDeliveryPoints $ code firstCity


-- A helper to define what "fresh" means (e.g., 6 hours)
isFresh :: UTCTime -> UTCTime -> Bool
isFresh now prev = let sixHours = 6 * 60 * 60 in diffUTCTime now prev < sixHours


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
data SdekPayment = SdekPayment { payValue :: Float } deriving (Show, Generic)

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


-- | Builds the minimal SdekOrderRequest payload needed to register an order.
--   Offloads address/item details to be filled in manually later.
buildMinimalOderRequest :: Text -> Text -> SdekOrderRequest
buildMinimalOderRequest name phone =
  let
    -- 1. Create the recipient payload
    recipient = SdekRecipient
      { rcpName = name
      , rcpPhones = [SdekPhone phone]
      }

    -- 2. Item info. Create a single, generic item for the fabric.
    item = SdekPackageItem
      { pkiName = "Ткань" -- A generic name is fine for your manual workflow
      , pkiWareKey = "111111" -- Use your internal fabric ID
      , pkiPayment = SdekPayment { payValue = 1.00 }
      , pkiWeight = 500 -- A sensible default weight in grams
      , pkiAmount = 1   -- It's one "item" (one piece of fabric)
      , pkiCost = 1
      }

    -- 3. Create a default package payload
    --    You MUST provide an estimated weight. You can't skip this.
    --    A sensible default (e.g., 500g) is a good start.
    package = SdekPackage
      { pkgNumber = "1" -- Simple default for one-package orders
      , pkgWeight = 1 -- Default weight in grams
      , pkgItems = [item]
      }
  in
    -- 3. Assemble the final request
    SdekOrderRequest
      { -- Hardcode the tariff for now if you only offer one type of delivery
        sorTariffCode = 137 -- e.g., "Посылка склад-ПВЗ"
      , sorRecipient = recipient
      , sorPackages = [package]
      , sorShipmentPoint = "shipment_point"
      , sorDeliveryPoint = "delivery_point"
      }


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

registerOrder :: SdekOrderRequest -> AppM (Either SdekError UUID)
registerOrder order = do
  url <- fmap (T.unpack . _sdekUrl) ask
  -- Step 1: Find the SDEK city code.
  $(logTM) InfoS $ logStr $ "registering order in sdek" <> show order
  let ordersUrl = "https://" <> url <> "/v2/orders"
  let ordersReq = getValidSdekToken >>= (_postReq' ordersUrl order . Just . sdekAccessToken)
  eOrders <- makeRequestWithRetries @SdekOrderResponse (Just (void $ getValidSdekToken)) ordersReq
  handleApiResponse @_ @SdekOrderResponse $(currentModule) eOrders $ \resp -> do
    when(sorRequestState resp == Accepted) $ 
      $(logTM) InfoS $ logStr $ "sdek has responded positively with uuid: " <> show (sorEntityUuid resp)
    return $
      case sorRequestState resp of
        Accepted -> do
          -- This is the happy path. We return the UUID to be used by the poller.
          Right $ sorEntityUuid resp
        Invalid ->
          -- The request was invalid. Now we check for the error details.
          Left $ case sorErrors resp of
            -- THIS IS YOUR NEW, ROBUST LOGIC:
            -- If the errors array is empty, we create our own, informative error.
            Just [] -> SdekError
                    { seCode = "INVALID_STATE_NO_DETAILS"
                    , seMessage = "SDEK reported an INVALID status but provided no error details."
                    }
            -- If there are errors, we just take the first one.
            Just (firstError:_) -> firstError
            Nothing -> SdekError 
                    { seCode = "INVALID ERROR STATE"
                    , seMessage = "status Invalid, but errors list is empty" 
                    }
        -- Handle other unexpected states gracefully
        otherState ->
          Left $ SdekError
            { seCode = "UNEXPECTED_STATE"
            , seMessage = "SDEK returned an unexpected initial state: " <> T.pack (show otherState)
            }