-- We'll need these extensions for Aeson and defining our types
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infrastructure.Services.DaData.StandardisedAddress where -- Example module name


import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON)
import Data.Scientific (Scientific, toBoundedInteger) -- Good for lat/lon
import Data.Aeson (defaultOptions, fieldLabelModifier, FromJSON(..), ToJSON(..), withScientific)

import Text (camelToSnake)
import API.WithField (WithField)



-- ==========================================================
--                    REQUEST TYPES
-- ==========================================================

-- A simple newtype for the raw address string we send.
-- The API expects an array of strings, so we will send '[Text]'.
-- We can enforce the single-item array in the client function.
newtype RawAddress = RawAddress Text
  deriving (Show, Eq, Generic, ToJSON)


-- ==========================================================
--                    RESPONSE TYPES
-- ==========================================================

-- | Represents the quality of the address match from the DaData API.
--   We model this as an ADT for maximum type safety.
data QualityCode
  = QC0_Verified          -- ^ qc = 0: Address recognized confidently. This is our only success case.
  | QC1_Uncertain         -- ^ qc = 1: Address has extra parts or is incomplete.
  | QC2_Garbage           -- ^ qc = 2: Address is empty or known garbage.
  | QC3_Ambiguous         -- ^ qc = 3: Multiple alternative addresses exist.
  | QC4_NotFound          -- ^ qc = 4: Address was not found at all.
  | QCOther Int           -- ^ A fallback for any other integer codes.
  deriving (Show, Eq, Generic)

-- We must write a manual FromJSON instance to parse the integer from the API
-- into our much safer ADT.
instance FromJSON QualityCode where
  parseJSON = withScientific "QualityCode" $ \n ->
    case toBoundedInteger @Int n of
      Just 0 -> pure QC0_Verified
      Just 1 -> pure QC1_Uncertain
      Just 2 -> pure QC2_Garbage
      Just 3 -> pure QC3_Ambiguous
      Just 4 -> pure QC4_NotFound
      Just other -> pure (QCOther other)
      Nothing -> fail "Expected an integer for quality code."


-- | This is the main, rich response object for a successful and verified (qc=0) address.
data DaDataAddress = DaDataAddress
  { result      :: Text
  , postalCode  :: Maybe Text -- Postal code can sometimes be missing
  , geoLat      :: Maybe Scientific
  , geoLon      :: Maybe Scientific
  } deriving (Show, Eq, Generic)

-- We use Aeson's Template Haskell to automatically derive the FromJSON instance,
-- which will correctly map 'postal_code' to 'postalCode', etc.
$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake } ''DaDataAddress)

type RawDaDataResponse = WithField "qc" QualityCode DaDataAddress

-- | This is the FINAL, high-level type your application should use.
--   It directly represents your business rule: "only qc=0 is a success."
data AddressVerificationResult
  -- The address was successfully verified (qc=0).
  = AddressVerified DaDataAddress
  -- The address was not verified for a specific reason.
  | AddressInvalid QualityCode
  deriving (Show, Eq)