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
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Time (UTCTime)

-- ===============================================================
-- 1. Shipment Status
--    Covers CDEK API v2 Webhook Statuses
-- ===============================================================
data SdekShipmentState
  -- Initial Stages
  = StatusCreated                           -- "CREATED"
  | StatusRemoved                           -- "REMOVED"
  
  -- Processing at Sender City
  | StatusAccepted                          -- "ACCEPTED"
  | StatusReceivedAtShipmentWarehouse       -- "RECEIVED_AT_SHIPMENT_WAREHOUSE"
  | StatusReadyForShipmentInSenderCity      -- "READY_FOR_SHIPMENT_IN_SENDER_CITY"
  | StatusTakenByTransporterFromSenderCity  -- "TAKEN_BY_TRANSPORTER_FROM_SENDER_CITY"
  | StatusReadyToShipAtSendingOffice        -- "READY_TO_SHIP_AT_SENDING_OFFICE"
  | StatusReadyForShipmentInTransitCity     -- "READY_FOR_SHIPMENT_IN_TRANSIT_CITY"
  | StatusReturnedToSenderCityWarehouse     -- "RETURNED_TO_SENDER_CITY_WAREHOUSE"
  | StatusReturnedToRecipientCityWarehouse  -- "RETURNED_TO_RECIPIENT_CITY_WAREHOUSE"
  
  -- Transit (The "Black Box" between cities)
  | StatusSentToTransitCity                 -- "SENT_TO_TRANSIT_CITY"
  | StatusAcceptedInTransitCity             -- "ACCEPTED_IN_TRANSIT_CITY"
  | StatusSentToRecipientCity               -- "SENT_TO_RECIPIENT_CITY"
  
  -- Destination City / Last Mile
  | StatusAcceptedAtDeliveryWarehouse       -- "ACCEPTED_AT_DELIVERY_WAREHOUSE"
  | StatusTakenByCourier                    -- "TAKEN_BY_COURIER" (Out for delivery)
  
  -- Final States
  | StatusAcceptedAtPickUpPoint             -- "ACCEPTED_AT_PICK_UP_POINT" (Arrived at PVZ)
  | StatusDelivered                         -- "DELIVERED"
  | StatusNotDelivered                      -- "NOT_DELIVERED" (Failed)
  | StatusReturned                          -- "RETURNED" (Sent back to sender)
  | StatusPostomatPosted
  | StatusPostomatReceived
  
  -- Fallback
  | StatusUnknown Text
  deriving (Show, Eq, Generic)

-- Custom Parser
instance FromJSON SdekShipmentState where
  parseJSON = withText "SdekShipmentState" $ \t -> return $ case t of
    -- Initial
    "CREATED"                                 -> StatusCreated
    "REMOVED"                                 -> StatusRemoved
    
    -- Sender Side
    "ACCEPTED"                                -> StatusAccepted
    "RECEIVED_AT_SHIPMENT_WAREHOUSE"          -> StatusReceivedAtShipmentWarehouse
    "READY_FOR_SHIPMENT_IN_SENDER_CITY"       -> StatusReadyForShipmentInSenderCity
    "TAKEN_BY_TRANSPORTER_FROM_SENDER_CITY"   -> StatusTakenByTransporterFromSenderCity
    "READY_FOR_SHIPMENT_IN_TRANSIT_CITY"      -> StatusReadyForShipmentInTransitCity
    "READY_TO_SHIP_AT_SENDING_OFFICE"         -> StatusReadyToShipAtSendingOffice
    "RETURNED_TO_SENDER_CITY_WAREHOUSE"       -> StatusReturnedToSenderCityWarehouse
    "RETURNED_TO_RECIPIENT_CITY_WAREHOUSE"    -> StatusReturnedToRecipientCityWarehouse
    
    -- Transit
    "SENT_TO_TRANSIT_CITY"                    -> StatusSentToTransitCity
    "ACCEPTED_IN_TRANSIT_CITY"                -> StatusAcceptedInTransitCity
    "SENT_TO_RECIPIENT_CITY"                  -> StatusSentToRecipientCity
    
    -- Destination Side
    "ACCEPTED_AT_DELIVERY_WAREHOUSE"          -> StatusAcceptedAtDeliveryWarehouse
    "ACCEPTED_AT_PICK_UP_POINT"               -> StatusAcceptedAtPickUpPoint
    "TAKEN_BY_COURIER"                        -> StatusTakenByCourier
    "POSTOMAT_POSTED"                         -> StatusPostomatPosted
    
    -- Final
    "POSTOMAT_RECEIVED"                       -> StatusPostomatReceived
    "DELIVERED"                               -> StatusDelivered
    "NOT_DELIVERED"                           -> StatusNotDelivered
    "RETURNED"                                -> StatusReturned
    
    -- Fallback
    other                                     -> StatusUnknown other

-- ===============================================================
-- 2. The Entity (Order Details)
--    Located in JSON: root.entity
-- ===============================================================
data SdekEntity = SdekEntity
  { entityUuid          :: UUID
  , entityCdekNumber    :: Maybe Text          -- The official Tracking Number (cdek_number)
  , entityCdekStatus    :: Maybe SdekShipmentState   -- The Physical Status
  , entityKeepFreeUntil :: Maybe UTCTime
  } deriving (Show, Eq, Generic)


instance FromJSON SdekEntity where
  parseJSON = withObject "SdekEntity" $ \v -> do
    u   <- v .: "uuid"
    num <- v .:? "cdek_number"
    keep <- v .:? "keep_free_until"
    
    -- 1. Extract the list of status objects (default to empty list if missing)
    statusList <- v .:? "statuses" .!= []
    
    -- 2. Determine the current status
    --    The API returns the list sorted by date DESC (Newest is index 0).
    --    We take the head of the list.
    let currentStatus = case statusList of
                          [] -> Nothing
                          (newestObj : _) -> 
                            -- 3. Extract the "code" field from the object 
                            --    and parse it into SdekShipmentState
                            parseMaybe (\o -> o .: "code") newestObj

    return $ SdekEntity u num currentStatus keep


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
  { respEntity        :: Maybe SdekEntity -- 'entity' might be null if UUID is wrong
  , respRequests      :: [SdekRequest]
  } deriving (Show, Eq, Generic)

instance FromJSON SdekOrderInTransitResponse where
  parseJSON = 
    withObject "SdekOrderInTransitResponse" $ \v ->
      SdekOrderInTransitResponse
      <$> v .:? "entity"
      <*> v .:? "requests" .!= []