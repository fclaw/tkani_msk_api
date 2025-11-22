{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Sdek.Types.OrderInTransit 
  ( SdekOrderInTransitResponse(..)
  , SdekEntity(..)
  , SdekShipmentState(..)
  , SdekRequest(..)
  , SdekRequestState(..)
  , SdekErrorObj(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)

-- ===============================================================
-- 1. Shipment Status (Physical Location)
--    Located in JSON: entity.cdek_status
-- ===============================================================
data SdekShipmentState
  = StatusCreated           -- "CREATED": Papers signed, box not given yet.
  | StatusAccepted          -- "ACCEPTED": Courier/Warehouse has the box.
  | StatusSent              -- "SENT": In transit between cities.
  | StatusArrived           -- "ARRIVED": At destination city.
  | StatusReadyForPickup    -- "READY_FOR_PICKUP": Waiting at PVZ.
  | StatusDelivered         -- "DELIVERED": Customer has it.
  | StatusNotDelivered      -- "NOT_DELIVERED": Issue/Return.
  | StatusUnknown Text      -- Future-proof for new SDEK statuses.
  deriving (Show, Eq, Generic)

-- Custom Parser to handle string mapping
instance FromJSON SdekShipmentState where
  parseJSON = withText "SdekShipmentState" $ \t -> return $ case t of
    "CREATED"          -> StatusCreated
    "ACCEPTED"         -> StatusAccepted
    "SENT"             -> StatusSent
    "ARRIVED"          -> StatusArrived
    "READY_FOR_PICKUP" -> StatusReadyForPickup
    "DELIVERED"        -> StatusDelivered
    "NOT_DELIVERED"    -> StatusNotDelivered
    other              -> StatusUnknown other

-- ===============================================================
-- 2. The Entity (Order Details)
--    Located in JSON: root.entity
-- ===============================================================
data SdekEntity = SdekEntity
  { entityUuid       :: UUID
  , entityCdekNumber :: Maybe Text          -- The official Tracking Number (cdek_number)
  , entityCdekStatus :: Maybe SdekShipmentState   -- The Physical Status
  } deriving (Show, Eq, Generic)

instance FromJSON SdekEntity where
  parseJSON = withObject "SdekEntity" $ \v -> SdekEntity
    <$> v .: "uuid"
    <*> v .:? "cdek_number" -- Optional, might not exist immediately on creation
    <*> v .:? "cdek_status"

-- ===============================================================
-- 3. Request State (API Validation Logic)
--    Located in JSON: root.requests[].state
-- ===============================================================
data SdekRequestState
  = ReqSuccessful 
  | ReqInvalid
  | ReqWaiting
  | ReqUnknown Text
  deriving (Show, Eq, Generic)

instance FromJSON SdekRequestState where
  parseJSON = withText "SdekRequestState" $ \t -> return $ case t of
    "SUCCESSFUL" -> ReqSuccessful
    "INVALID"    -> ReqInvalid
    "WAITING"    -> ReqWaiting
    other        -> ReqUnknown other

-- ===============================================================
-- 4. Errors Object
--    Located in JSON: root.requests[].errors[]
-- ===============================================================
data SdekErrorObj = SdekErrorObj
  { errCode    :: Maybe Text
  , errMessage :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON SdekErrorObj where
  parseJSON = withObject "SdekErrorObj" $ \v -> SdekErrorObj
    <$> v .:? "code"
    <*> v .:? "message"

-- ===============================================================
-- 5. The Request History Item
--    Located in JSON: root.requests[]
-- ===============================================================
data SdekRequest = SdekRequest
  { reqUuid      :: UUID
  , reqState     :: SdekRequestState
  , reqErrors    :: [SdekErrorObj]
  , reqWarnings  :: [SdekErrorObj]
  } deriving (Show, Eq, Generic)

instance FromJSON SdekRequest where
  parseJSON = withObject "SdekRequest" $ \v -> SdekRequest
    <$> v .: "request_uuid"
    <*> v .: "state"
    -- Handle missing error arrays gracefully by defaulting to empty list
    <*> v .:? "errors"   .!= []
    <*> v .:? "warnings" .!= []

-- ===============================================================
-- 6. Top Level Response
--    The type returned by your getOrdersInTransit function
-- ===============================================================
data SdekOrderInTransitResponse = SdekOrderInTransitResponse
  { respEntity   :: Maybe SdekEntity -- 'entity' might be null if UUID is wrong
  , respRequests :: [SdekRequest]
  } deriving (Show, Eq, Generic)

instance FromJSON SdekOrderInTransitResponse where
  parseJSON = withObject "SdekOrderInTransitResponse" $ \v -> SdekOrderInTransitResponse
    <$> v .:? "entity"
    <*> v .:? "requests" .!= []