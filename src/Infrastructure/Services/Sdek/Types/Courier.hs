{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-} -- Useful for fields like 'type' or 'errors'
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Sdek.Types.Courier where

import Data.Aeson (FromJSON(..), genericParseJSON, withObject, (.:), (.:?), defaultOptions)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.UUID (UUID)
import Data.Maybe (listToMaybe)

import Infrastructure.Services.Sdek.Types.Error (SdekErrorDetail) -- Re-using the error type from before
import Infrastructure.Services.Sdek.Types.State (SdekRequestState)

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
data SdekCourierResponse = SdekCourierResponse
  { entity :: SdekCourierResponseEntity
  , requests :: [SdekRequestDto]
  } deriving (Show, Eq, Generic)

instance FromJSON SdekCourierResponse where
  parseJSON = genericParseJSON defaultOptions


data SdekPickupApplicationResponse = 
     SdekPickupApplicationResponse 
     { state :: SdekRequestState 
     , errors :: Maybe [SdekErrorDetail]
     }   deriving (Show, Eq, Generic)

-- | Manual FromJSON instance to navigate the nested structure.
instance FromJSON SdekPickupApplicationResponse where
  parseJSON = withObject "SdekPickupApplicationResponse" $ \topLevel -> do
     -- 1. Go into the 'requests' array
    requests <- topLevel .: "requests"
    -- 2. Safely get the first element of the 'requests' array
    case listToMaybe requests of
      -- If the array is empty, the parse fails.
      Nothing -> fail "The 'requests' array is empty."
      -- If there's at least one element...
      Just firstRequest ->
        -- 3. ...go into that object and extract the 'state' field.
        withObject "RequestDto" (\req -> SdekPickupApplicationResponse <$> req .: "state" <*> req .:? "errors") firstRequest