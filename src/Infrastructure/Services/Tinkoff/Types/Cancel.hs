{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Infrastructure.Services.Tinkoff.Types.Cancel where


import Data.Aeson.TH
import Data.Int (Int64)
import Data.Text (Text)

import Text (recordLabelModifierG, pascalCase)


-- | The request body for the /v2/Cancel endpoint.
data CancelRequest = CancelRequest
  { cTerminalKey :: Text
  , cPaymentId   :: Text
  , cToken       :: Text -- The signature token
  } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG pascalCase "c"} ''CancelRequest)

-- | Represents the response from the T-Bank /v2/Cancel endpoint.
data CancelResponse = CancelResponse
  { -- | Indicates if the API call was successful.
    cSuccess    :: Bool
    -- | "0" on success. A non-zero string on failure.
  , cErrorCode  :: Text
    -- | A detailed error message. Usually present on failure.
  , cMessage    :: Maybe Text
  } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG pascalCase "c"} ''CancelResponse)