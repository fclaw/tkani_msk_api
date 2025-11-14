-- We need a few language extensions for this.
{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic
{-# LANGUAGE DuplicateRecordFields #-} -- To allow fields like 'priceRub' in multiple records
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Types 
  (PreCut, 
   FabricInfo (..), 
   ApiError (..), 
   ApiResponse,
   FullFabric,
   Providers (..),
   DeliveryPoint (..),
   ProviderInfo (..),
   OrderRequest (..),
   OrderStatus (..),
   DisplayInfo (..),
   PointLocation (..),
   OrderConfirmationDetails (..),
   defOrderConfirmationDetails,
   mkError) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), Value(..), withObject)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower)
import GHC.Generics (Generic)
import Data.Text (pack)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import Data.Aeson.TH

import API.WithField (WithField)
import Text (camelToSnake, recordLabelModifier) 


-- | A standard structure for an error response.
data ApiError = ApiError
  { errorCode    :: Text -- A machine-readable error code, e.g., "product_not_found"
  , errorMessage :: Text -- A human-readable message
  } deriving (Show, Generic)

-- We can use the default Generic instances for ApiError
instance ToJSON ApiError
instance FromJSON ApiError

-- | A standard structure for a success response.
--   The 'a' is the type of the actual data payload.
data ApiSuccess a = ApiSuccess
  { apiData :: a
  } deriving (Show, Generic)

instance ToJSON a => ToJSON (ApiSuccess a)
instance FromJSON a => FromJSON (ApiSuccess a)

-- | The main response type that can be EITHER a success OR an error.
type ApiResponse a = Either ApiError a

-- | The Custom ToJSON instance. This is where the magic happens.
--   This tells Aeson how to convert our 'ApiResponse a' into JSON.
instance {-# OVERLAPPING #-} ToJSON a => ToJSON (ApiResponse a) where
  toJSON (Left apiError) =
    -- If it's an error, create an object like: {"ok": false, "error": {...}}
    object [ "ok"    .= False
           , "error" .= toJSON apiError
           ]
  toJSON (Right successData) =
    -- If it's a success, create an object like: {"ok": true, "ok": {...}}
    -- Wait, this is a typo. The key for successData should not be "ok". It should be something like "data".
    -- Let's correct it.
    object [ "ok"   .= True
           , "data" .= toJSON successData -- Let's use "data" for the payload
           ]


mkError e = (ApiError mempty e)


-- | Represents a specific, fixed-length pre-cut of a fabric.
--   This corresponds to the 'pre_cuts' table in the database.
data PreCut = PreCut
  { -- We use prefixes like 'pc' to avoid name collisions.
    pcLengthM :: Double  -- Corresponds to 'length_m'
  , pcPriceRub:: Int     -- Corresponds to 'price_rub' (in kopecks/cents)
  , pcInStock :: Bool    -- Corresponds to 'in_stock'
  } deriving (Show, Generic)

-- Automatically derive the necessary instances
instance ToJSON PreCut
instance FromJSON PreCut


-- | Represents the full information for a fabric type, including any
--   available pre-cuts. This is a combined view, not a direct table mapping.
data FabricInfo = FabricInfo
  { -- Fabric Properties (from the 'fabrics' table)
    fiDescription        :: Text
  , fiTotalLengthM       :: Int
  , fiPricePerMeter      :: Int    -- In kopecks/cents
    -- Inventory Status
  , fiAvailableLengthM   :: Double -- Amount available for "cut-to-order"
    -- List of associated pre-cuts for this fabric.
  , fiPreCuts            :: Maybe [WithField "pcId" Int PreCut]
  } deriving (Show, Generic)

-- Automatically derive the necessary instances
instance ToJSON FabricInfo
instance FromJSON FabricInfo

type FullFabric = WithField "fiIsSold" Bool (WithField "fiId" Int FabricInfo)


data Providers = SDEK | NONE
  deriving (Show, Eq, Read)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''Providers)

-- Convert from URL path segment TO our Providers type
instance FromHttpApiData Providers where
  parseUrlPiece text =
    -- We'll make it case-insensitive for robustness
    case T.toLower text of
      "sdek"     -> Right SDEK
      _          -> Left "Unknown provider"

-- Convert from our Providers type TO a URL path segment
instance ToHttpApiData Providers where
  toUrlPiece provider =
    case provider of
      SDEK     -> "sdek"

    
data DeliveryPoint = DeliveryPoint
  {   dpCode            :: Text
    , dpName            :: Text
    , dpWorkTime        :: Text
    , dpHasDressingRoom :: Bool
    , dpLocation        :: PointLocation
    , dpDisplay         :: DisplayInfo
  } deriving (Show, Generic)

data PointLocation = PointLocation
  { locAddressFull :: Text
  , locLongitude   :: Double
  , locLatitude    :: Double
  } deriving (Show, Generic)

data DisplayInfo = DisplayInfo
  { diButtonText :: Text
  , diMessageText :: Text
  } deriving (Show, Generic)


instance ToJSON PointLocation
instance FromJSON PointLocation

instance ToJSON DisplayInfo
instance FromJSON DisplayInfo

instance ToJSON DeliveryPoint
instance FromJSON DeliveryPoint

data ProviderInfo = ProviderInfo { piCode :: Text, piName :: Text }
  deriving (Show, Generic)

-- Make it encodable to JSON for the API response
instance ToJSON ProviderInfo where
  toJSON (ProviderInfo code name) = object ["code" .= code, "name" .= name]

-- Make it decodable from YAML/JSON
instance FromJSON ProviderInfo where
  parseJSON = withObject "ProviderInfo" $ \v -> ProviderInfo <$> v .: "code" <*> v .: "name"


-- We'll assume these types are defined elsewhere
-- data Fabric = Fabric { fabricId :: Int, ... }
-- type DeliveryProviderId = Text -- e.g., "sdek", "boxberry"
-- type DeliveryPointId = Text    -- e.g., "sdek_EKB20"

-- | Represents a customer order request before payment and confirmation.
-- This is the data structure your bot will build and send to the API.
data OrderRequest = OrderRequest
  { -- Core Product Information
    orFabricId      :: Int          -- The ID of the fabric being ordered. REQUIRED.

    -- Purchase Details (exactly one of these should be Just)
  , orLengthM       :: Maybe Double   -- Length in meters for a custom cut.
  , orPreCutId      :: Maybe Int      -- The ID of the specific pre-cut piece.

    -- Customer & Delivery Information
  , orCustomerFullName :: Text       -- Full name as a single string (e.g., "Иванов Иван Иванович").
  , orCustomerPhone    :: Text       -- Phone number, normalized (e.g., "+79211234567").
  
  , orDeliveryProviderId :: Providers          -- The code for the provider, e.g., "sdek".
  , orDeliveryPointId    :: Text    -- The unique ID of the chosen delivery point.

   -- link to telegram where the fabric image and description is situated
  , orTelegramUrl :: Text
  } deriving (Show, Generic)

-- We'll need ToJSON/FromJSON instances for this to be sent over the API
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "or" } ''OrderRequest)

-- | Represents the lifecycle stages of an order.
data OrderStatus
  = New                 -- Order created by the bot, awaiting payment.
  | Paid                -- Payment received, awaiting fulfillment.
  | PreparedForDelivery -- Order packed, label printed, awaiting courier pickup.
  | OnRoute             -- Courier has picked up the package, it's in transit.
  | Delivered           -- Customer has received the package.
  deriving (Show, Eq, Read, Bounded, Enum, Generic)


$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake } ''OrderStatus)

-- A record to hold all the necessary information for the final confirmation.
data OrderConfirmationDetails = OrderConfirmationDetails
  { orderId          :: Text -- e.g., "T-20231114-A4B7" - CRUCIAL for support
  , purchasedItems   :: [(Text, Int)] -- List of (Fabric Name, Quantity/Length)
  , totalAmount      :: Float  -- e.g., 125.50
  , paymentLink      :: Text
  }

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake } ''OrderConfirmationDetails)


defOrderConfirmationDetails = OrderConfirmationDetails mempty [] 0.0 mempty