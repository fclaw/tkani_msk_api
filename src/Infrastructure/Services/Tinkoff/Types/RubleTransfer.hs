{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic

module Infrastructure.Services.Tinkoff.Types.RubleTransfer where

import Data.Aeson (ToJSON)
import Data.Aeson.TH
import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

import Text (recordLabelModifierG, firstToLower)


-- =============================================================================
-- Sub-structures
-- =============================================================================



-- | Payer Details
data Payer = Payer
  { paAccountNumber :: Text -- ^ Matches "accountNumber" in JSON
  } deriving (Show, Eq, Generic)

-- Prefix: "pa" (Payer)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG firstToLower "pa" } ''Payer)


-- | Receiver Details
data Receiver = Receiver
  { reName          :: Text
  , reInn           :: Text
  , reKpp           :: Maybe Text
  , reBik           :: Text
  , reAccountNumber :: Text       -- ^ Recipient checking account (requested)
  , reBankName      :: Maybe Text
  , reCorrAccount   :: Maybe Text -- ^ Optional: correspondent account
  } deriving (Show, Eq, Generic)

-- Prefix: "re" (Receiver)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG firstToLower "re" } ''Receiver)

-- =============================================================================
-- Root Request: Ruble Transfer
-- ================================= :> "pay"
-- =============================================================================

data RubleTransferRequest = RubleTransferRequest
  { rtId              :: Text          -- ^ Unique identifier (<= 64 chars), RECOMMENDED: UUID v4
  , rtFrom            :: Payer         -- ^ Your checking account
  , rtTo              :: Receiver      -- ^ Recipient (Ya/Sdek)
  , rtPurpose         :: Text          -- ^ Payment narrative
  , rtAmount          :: Double        -- ^ Amount in RUB (not kopecks!)
  } deriving (Show, Eq, Generic)

-- Prefix: "rt" (RubleTransfer)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG firstToLower "rt" } ''RubleTransferRequest)

-- =============================================================================
-- Success Response (201 Created)
-- =============================================================================

-- Represented as () in the service layer since the body is empty.

-- =============================================================================
-- Error Response (400, 401, 403, 422, 429, 500)
-- =============================================================================

data ErrorDetails = ErrorDetails
  { edReason :: Maybe Text -- ^ Specific reason (e.g. "Check recipient account number")
  } deriving (Show, Eq, Generic)

-- Prefix: "ed"
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG firstToLower "ed" } ''ErrorDetails)

data TBankError = TBankError
  { teErrorId      :: Text             -- ^ Trace ID for support (e.g. "asdq3412")
  , teErrorCode    :: Text             -- ^ Internal code (e.g. "VALIDATION_ERROR")
  , teErrorMessage :: Text             -- ^ Human readable message
  , teErrorDetails :: Maybe ErrorDetails -- ^ Only present in 422 errors
  } deriving (Show, Eq, Generic)

-- Prefix: "te"
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG firstToLower "te" } ''TBankError)


-- | THE MASTER RESPONSE WRAPPER
-- Just TBankError -> Something went wrong (4xx/5xx)
-- Nothing         -> Request was accepted (201 Created)
data RubleTransferResponse = RubleTransferResponse { rtrError :: Maybe TBankError }
  deriving (Show, Eq, Generic)

-- Prefix: "rtr" (RubleTransferResponse)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG firstToLower "rtr" } ''RubleTransferResponse)

data RubleTransferStatusRequest = RubleTransferStatusRequest
  { rtsrId :: Text -- ^ The same ID you used in the original transfer request
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG firstToLower "rtsr" } ''RubleTransferStatusRequest)

statusRequestToParam :: RubleTransferStatusRequest -> Text
statusRequestToParam req = rtsrId req

-- Requirements: [IN_PROGRESS, EXECUTED, FAILED, CANCELLED]
data TransferStatus = IN_PROGRESS | EXECUTED | FAILED | CANCELLED
  deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = recordLabelModifierG id mempty } ''TransferStatus)

data RubleTransferStatusResponse = RubleTransferStatusResponse
  { rtsrStatus :: TransferStatus
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG firstToLower "rtsr" } ''RubleTransferStatusResponse)