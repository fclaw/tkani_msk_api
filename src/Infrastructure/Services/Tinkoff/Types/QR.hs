{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic


module Infrastructure.Services.Tinkoff.Types.QR where


import Data.Aeson (ToJSON)
import Data.Aeson.TH
import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Text (Text)

import Text (recordLabelModifierG, pascalCase)
import Infrastructure.Services.Tinkoff.Types.Enum


-- | The request body for the /v2/GetQr endpoint.
data GetQrRequest = GetQrRequest
  { gqrTerminalKey :: Text
  , gqrPaymentId   :: Int64
  , gqrDataType    :: QrDataType
  , gqrToken       :: Text -- The signature token
  } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG pascalCase "gqr"} ''GetQrRequest)

defGetQrRequest = GetQrRequest mempty 0 PAYLOAD mempty

-- | Represents the response from the T-Bank /v2/GetQr endpoint.
data GetQrResponse = GetQrResponse
  { -- | Indicates if the API call was successful.
    gqrrSuccess    :: Bool
    -- | "0" on success. A non-zero string on failure.
  , gqrrErrorCode  :: Text
    -- | A detailed error message. Usually present on failure.
  , gqrrMessage    :: Maybe Text
    -- | The unique ID of the payment session.
  , gqrrPaymentId  :: Maybe Int64
    -- | The data for the QR code. This will be either a URL string (for PAYLOAD)
    --   or an SVG image string (for IMAGE).
  , gqrrData       :: Maybe Text
  } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG pascalCase "gqrr"} ''GetQrResponse)