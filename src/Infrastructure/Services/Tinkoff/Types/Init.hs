{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic


module Infrastructure.Services.Tinkoff.Types.Init where


import Data.Aeson (ToJSON)
import Data.Aeson.TH
import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Text (Text)

import Text (camelToSnake, recordLabelModifierG, pascalCase)


import Infrastructure.Services.Tinkoff.Types.Enum

-- | Represents a single item in the fiscal receipt.
data ReceiptItem = ReceiptItem
  { riName          :: Text
  , riPrice         :: Int64         -- Price of one item, in kopecks
  , riQuantity      :: Double        -- Quantity (e.g., 1.5 for meters of fabric)
  , riAmount        :: Int64         -- Total cost for this line (Price * Quantity)
  , riTax           :: Tax
  , riPaymentObject :: PaymentObject
  , riPaymentMethod :: PaymentMethod
  } deriving (Show, Eq, Generic)

-- Custom ToJSON for PascalCase
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG pascalCase "ri"} ''ReceiptItem)

-- | Represents the fiscal receipt object.
data ReceiptData = ReceiptData
  { rdEmail         :: Maybe Text
  , rdPhone         :: Maybe Text
  , rdTaxation      :: Taxation
  , rdItems         :: [ReceiptItem]
  } deriving (Show, Eq, Generic)


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG pascalCase "rd" } ''ReceiptData)

defReceiptData = ReceiptData mempty mempty UsnIncome []

-- | Optional customer data.
data CustomerData = CustomerData
  { cdPhone         :: Maybe Text
  , cdEmail         :: Maybe Text
  } deriving (Show, Eq, Generic)


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG pascalCase "cd"} ''CustomerData)

defCustomerData = CustomerData mempty mempty

-- | The main request body for the /v2/Init endpoint.
data InitRequest = InitRequest
  { irTerminalKey   :: Text
  , irAmount        :: Int64  -- Total amount of the order, in kopecks
  , irOrderId       :: Text
  , irDescription   :: Maybe Text
  , irToken         :: Text   -- The generated SHA-256 signature
  , irData          :: Maybe CustomerData
  , irReceipt       :: Maybe ReceiptData
  } deriving (Show, Eq, Generic)



-- Special ToJSON to handle the 'DATA' key which is all caps.
$(deriveJSON 
  defaultOptions 
  { fieldLabelModifier = 
      \fieldName -> 
         let baseName = recordLabelModifierG id "ir" fieldName
         in if baseName == "Data" -- Check if the stripped name is "Data"
            then "DATA"          -- If so, use all caps
            else baseName {- Otherwise, use as it is -} } ''InitRequest)


-- | Represents the response from the T-Bank /v2/Init endpoint.
--   We only decode the fields that are critical to our application logic.
--   Aeson will safely ignore other fields like "TerminalKey", "Status", etc.
data InitResponse = InitResponse
  { -- | Indicates if the API call was successful. Always check this first.
    irSuccess     :: Bool
    -- | "0" on success. A non-zero string on failure.
  , irErrorCode    :: Text
    -- | A detailed error message. Only present on failure.
  , irMessage      :: Maybe Text
    -- | The URL for the customer to complete the payment. Only present on success.
  , irPaymentURL   :: Maybe Text
    -- | The unique payment ID from Tinkoff. Store this for status polling.
  , irPaymentId    :: Maybe Text
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG pascalCase "ir"} ''InitResponse)
