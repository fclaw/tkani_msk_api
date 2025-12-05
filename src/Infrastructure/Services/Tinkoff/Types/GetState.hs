{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generi

module Infrastructure.Services.Tinkoff.Types.GetState (Status (..), GetStateRequest (..), GetStateResponse(..)) where


import Data.Aeson (ToJSON, FromJSON, withText, parseJSON, toJSON)
import Data.Aeson.TH
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Char (toLower)

import Text (camelToSnake, recordLabelModifierG, pascalCase)

-- | Represents the payment status returned by the Tinkoff GetState API.
data Status
 =  NEW              -- Payment session initiated.
  | PENDING
  | FORM_SHOWED
  | AUTHORIZING
  | CANCELLED         -- Payment was cancelled by the merchant or user.
  | CONFIRMED        -- One-step payment completed successfully.
  | AUTHORIZED       -- Two-step payment money held successfully.
  | REJECTED         -- Payment was rejected by the payment system or issuer.
  | DEADLINE_EXPIRED -- Payment session timed out.
  | REVERSED         -- Payment was fully reversed (refunded).
  | PARTIAL_REVERSED
  | REFUNDED
  -- A constructor for any status we don't recognize to prevent parsing failures.
  | UNKNOWN_STATUS Text
  deriving (Show, Eq)

instance FromJSON Status where
  parseJSON = withText "Status" $ \t -> return $ case t of
    "NEW"              -> NEW
    "PENDING"          -> PENDING
    "FORM_SHOWED"      -> FORM_SHOWED
    "AUTHORIZING"      -> AUTHORIZING
    "CANCELLED"        -> CANCELLED
    "CONFIRMED"        -> CONFIRMED
    "AUTHORIZED"       -> AUTHORIZED
    "REJECTED"         -> REJECTED
    "DEADLINE_EXPIRED" -> DEADLINE_EXPIRED
    "REVERSED"         -> REVERSED
    "PARTIAL_REVERSED" -> PARTIAL_REVERSED
    "REFUNDED"         -> REFUNDED
    other              -> UNKNOWN_STATUS other

instance ToJSON Status where
  toJSON s = case s of
    NEW              -> "NEW"
    PENDING          -> "PENDING"
    FORM_SHOWED      -> "FORM_SHOWED"
    AUTHORIZING      -> "AUTHORIZING"
    CANCELLED        -> "CANCELLED"
    CONFIRMED        -> "CONFIRMED"
    AUTHORIZED       -> "AUTHORIZED"
    REJECTED         -> "REJECTED"
    DEADLINE_EXPIRED -> "DEADLINE_EXPIRED"
    REVERSED         -> "REVERSED"
    PARTIAL_REVERSED -> "PARTIAL_REVERSED"
    REFUNDED         -> "REFUNDED"
    UNKNOWN_STATUS t -> toJSON t

-- | The request body for the /v2/GetState endpoint.
data GetStateRequest = GetStateRequest
  { -- | Your public terminal identifier.
    gsrqTerminalKey :: Text
    -- | The unique payment ID from Tinkoff, which you stored in your 'payments' table.
  , gsrqPaymentId   :: Text
    -- | The SHA-256 signature for this request.
  , gsrqToken       :: Text
    -- | Optional: The customer's IP address. While shown in the docs,
    --   it's often not strictly required for server-to-server polling.
  , gsrqIP          :: Maybe Text
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG id "gsrq" } ''GetStateRequest)

-- | Represents the response from the /v2/GetState endpoint.
--   We decode only the fields relevant to our poller.
data GetStateResponse = GetStateResponse
  { -- | Was the API call itself successful?
    gsrpSuccess   :: Bool
    -- | The current status of the payment. This is the most important field.
  , gsrpStatus    :: Maybe Status
    -- | "0" on success.
  , gsrpErrorCode :: Maybe Text
    -- | Your original order ID. Useful for matching.
  , gsrpOrderId   :: Maybe Text
    -- | The unique payment ID from Tinkoff.
  , gsrpPaymentId :: Maybe Text
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG id "gsrp" } ''GetStateResponse)