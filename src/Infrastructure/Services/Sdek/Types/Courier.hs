{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-} -- Useful for fields like 'type' or 'errors'
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Infrastructure.Services.Sdek.Types.Courier where

import Data.List (sortBy)
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.UUID (UUID)
import Data.Ord (comparing)
import Data.Maybe (listToMaybe)
import Data.Aeson.TH
import Data.Aeson

import Text (recordLabelModifier)
import Infrastructure.Services.Sdek.Types.Error (SdekErrorDetail) -- Re-using the error type from before
import Infrastructure.Services.Sdek.Types.State (SdekRequestState)


-- Your ADT for SDEK Pickup Application Status (assuming these map 1:1 to SDEK codes)
data SdekPickupAppStatus
  = ACCEPTED
  | CREATED
  | REMOVED
  | READY_FOR_APPOINTMENT
  | APPOINTED_COURIER
  | DONE
  | PROBLEM_DETECTED
  | PROCESSING_REQUIRED
  | INVALID
  deriving (Show, Eq, Generic) -- Make sure it derives Generic for Aeson

instance FromJSON SdekPickupAppStatus where
  parseJSON = withText "SdekPickupAppStatus" $ \s ->
    case s of
      "READY_FOR_APPOINTMENT" -> pure READY_FOR_APPOINTMENT
      "APPOINTED_COURIER"     -> pure APPOINTED_COURIER
      "DONE"                  -> pure DONE
      "PROBLEM_DETECTED"      -> pure PROBLEM_DETECTED
      "PROCESSING_REQUIRED"   -> pure PROCESSING_REQUIRED
      "INVALID"               -> pure INVALID
      "ACCEPTED"              -> pure ACCEPTED
      "CREATED"               -> pure CREATED
      "REMOVED"               -> pure REMOVED
      _                       -> fail $ "Unknown SDEK Pickup Application Status: " <> unpack s

instance ToJSON SdekPickupAppStatus where
  toJSON s = 
    case s of
      READY_FOR_APPOINTMENT -> String "READY_FOR_APPOINTMENT"
      APPOINTED_COURIER     -> String "APPOINTED_COURIER"
      DONE                  -> String "DONE"
      PROBLEM_DETECTED      -> String "PROBLEM_DETECTED"
      ACCEPTED              -> String "ACCEPTED"
      CREATED               -> String "CREATED"
      REMOVED               -> String "REMOVED"
      PROCESSING_REQUIRED   -> String "PROCESSING_REQUIRED"
      INVALID               -> String "INVALID"


-- This helper assigns a numerical rank to each status. Higher rank means "more advanced".
statusRank :: SdekPickupAppStatus -> Int
statusRank status = case status of
  INVALID               -> 0
  PROCESSING_REQUIRED   -> 10
  PROBLEM_DETECTED      -> 15 -- This is an "out-of-band" state, often possible from anywhere
  ACCEPTED              -> 20
  READY_FOR_APPOINTMENT -> 30
  APPOINTED_COURIER     -> 40
  REMOVED               -> 90  -- REMOVED is often terminal. We could give it high priority.
  DONE                  -> 100
  CREATED               -> 200 -- Often initial or successful validation (like ACCEPTED, but done)

-- | Represents a single request status object within the main response.
data SdekRequestDto = SdekRequestDto
  { requestUuid :: UUID
  , type'       :: Text
  , dateTime    :: UTCTime
  , state       :: SdekRequestState
  , errors      :: Maybe [SdekErrorDetail]
  , warnings    :: Maybe [SdekErrorDetail] -- Assuming WarningDto has same structure as ErrorDto
  } deriving (Show, Eq, Generic)

-- Custom JSON parser to handle 'type\'' -> 'type'
instance FromJSON SdekRequestDto where
  parseJSON = withObject "SdekRequestDto" $ \v -> SdekRequestDto
    <$> v .: "request_uuid"
    <*> v .: "type"
    <*> v .: "date_time"
    <*> v .: "state"
    <*> v .:? "errors"
    <*> v .:? "warnings"

-- | Represents the main 'entity' object in the courier call response.
data SdekCourierResponseEntity = SdekCourierResponseEntity
  { uuid :: UUID
  } deriving (Show, Eq, Generic)

instance FromJSON SdekCourierResponseEntity where
  parseJSON = 
    withObject "SdekCourierResponseEntity" $ 
      \v -> SdekCourierResponseEntity
        <$> v .: "uuid"

-- | The top-level response structure.
data SdekCourierResponse = 
     SdekCourierResponse
     { entity   :: SdekCourierResponseEntity
     , requests :: [SdekRequestDto]
     } deriving (Show, Eq, Generic)

instance FromJSON SdekCourierResponse where
  parseJSON = genericParseJSON defaultOptions

data SdekPickupAppStatusBody = SdekPickupAppStatusBody { statusCode :: SdekPickupAppStatus, statusDateTime :: UTCTime }
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "status" } ''SdekPickupAppStatusBody)

data SdekPickupApplicationResponse = 
     SdekPickupApplicationResponse
     { sparStatus :: SdekPickupAppStatus
     , sparState  :: SdekRequestState 
     , sparErrors :: Maybe [SdekErrorDetail]
     }   deriving (Show, Eq, Generic)

-- | Manual FromJSON instance to navigate the nested structure.
instance FromJSON SdekPickupApplicationResponse where
  parseJSON = withObject "SdekPickupApplicationResponse" $ \topLevel -> do
     -- extract the init status
    entity <- topLevel .: "entity"
    statuses :: [SdekPickupAppStatusBody] <- entity .: "statuses"
    let status = 
          case listToMaybe statuses of
            Nothing -> error "The 'statuses' array is empty."
            Just SdekPickupAppStatusBody {..} -> statusCode

     -- 1. Go into the 'requests' array
    requests <- topLevel .: "requests"
    -- 2. Safely get the first element of the 'requests' array
    case listToMaybe requests of
      -- If the array is empty, the parse fails.
      Nothing -> fail "The 'requests' array is empty."
      -- If there's at least one element...
      Just firstRequest ->
        -- 3. ...go into that object and extract the 'state' field.
        withObject "RequestDto" (\req -> SdekPickupApplicationResponse status <$> req .: "state" <*> req .:? "errors") firstRequest

newtype SdekPickupAppStatusResponse = SdekPickupAppStatusResponse { status :: SdekPickupAppStatus }
  deriving (Show, Eq, Generic)


instance FromJSON SdekPickupAppStatusResponse where
  parseJSON = withObject "SdekPickupAppStatusResponse" $ \topLevel -> do
    entity <- topLevel .: "entity"
    statuses :: [SdekPickupAppStatusBody] <- entity .: "statuses"
    let sorted_status = sortBy (flip (comparing statusDateTime)) statuses
    case listToMaybe sorted_status of
      Nothing -> fail "The 'statuses' array is empty."
      Just SdekPickupAppStatusBody {..} -> 
        pure $ SdekPickupAppStatusResponse statusCode