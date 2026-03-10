{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Infrastructure.Services.Yandex.Order where

import Data.Int (Int32)
import           Data.Aeson.TH
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Data.Aeson          (ToJSON (..), Options (..), defaultOptions, genericToJSON)

import Text (recordLabelModifier)
import Infrastructure.Services.Yandex.Types.Enums


-- =============================================================================
-- 1. Info & Idempotency
-- =============================================================================

data RequestInfo = RequestInfo
  { riOperatorRequestId :: Text        -- ^ MUST BE UNIQUE per order (Idempotency key). E.g., your DB order_id
  , riComment           :: Maybe Text  -- ^ Optional comment for the courier/point
  } deriving (Show, Eq, Generic)


defRequestInfo = RequestInfo mempty Nothing

-- =============================================================================
-- 2. Locations (Source & Destination)
-- =============================================================================

-- | Represents either your warehouse (Source) or the buyer (Destination).
-- Usually, you only provide 'platformStation' if delivering to a PVZ.
data DestinationRequestNode = DestinationRequestNode
  { drnType            :: NodeType               -- ^ "custom_location" (door) OR "platform_station" (PVZ)
  , drnCustomLocation  :: Maybe CustomLocation   -- ^ Fill this if delivering to a door / sending from your warehouse
  , drnPlatformStation :: Maybe PlatformStation  -- ^ Fill this if sending to a PVZ
  } deriving (Show, Eq, Generic)


defDestinationRequestNode = DestinationRequestNode Infrastructure.Services.Yandex.Types.Enums.PlatformStation Nothing Nothing

data SourceRequestNode = SourceRequestNode { srnPlatformStation :: PlatformStation } deriving (Show, Eq, Generic)

data PlatformStation = PlatformStation
  { psPlatformId :: Text -- ^ The exact PVZ UUID from your 2.02 endpoint list (e.g. "019bc37657...")
  } deriving (Show, Eq, Generic)

data CustomLocation = CustomLocation
  { clLatitude  :: Double
  , clLongitude :: Double
  , clDetails   :: LocationDetails
  } deriving (Show, Eq, Generic)


data LocationDetails = LocationDetails
  { ldFullAddress :: Text
  , ldRoom        :: Maybe Text -- ^ Apartment/Office
  } deriving (Show, Eq, Generic)


-- =============================================================================
-- 3. Billing & Payment
-- =============================================================================

data BillingInfo = BillingInfo
  { biPaymentMethod :: PaymentMethod -- ^ "already_paid", "card_on_receipt", or "cash_on_receipt"
  , biDeliveryCost  :: Int32  -- ^ Who pays for delivery? 0 if you provide free delivery to client. (usually in kopecks/minor units)
  } deriving (Show, Eq, Generic)

defBillingInfo = BillingInfo CardOnReceipt 0

-- =============================================================================
-- 4. Cargo (Items & Places/Boxes)
-- =============================================================================

data Item = Item
  { iCount          :: Int
  , iName           :: Text
  , iArticle        :: Text -- ^ SKU or Product Code
  , iBillingDetails :: ItemBillingDetails
  , iPlaceBarcode   :: Text
  } deriving (Show, Eq, Generic)


data ItemBillingDetails = ItemBillingDetails
  { ibdUnitPrice         :: Int32 -- ^ Price to be paid by customer (in kopecks/minor units, 100 = 1 ruble)
  , ibdAssessedUnitPrice :: Int32 -- ^ Declared value for insurance (in kopecks/minor units)
  } deriving (Show, Eq, Generic)

data Place = Place
  { pPhysicalDims :: PhysicalDimensions
  , pBarcode      :: Text
  } deriving (Show, Eq, Generic)

data PhysicalDimensions = PhysicalDimensions
  { pdDx          :: Int32 -- ^ Length in cm
  , pdDy          :: Int32 -- ^ Width in cm
  , pdDz          :: Int32 -- ^ Height in cm
  , pdWeightGross :: Int32 -- ^ Weight in Grams
  } deriving (Show, Eq, Generic)

defPlace = Place (PhysicalDimensions 0 0 0 0) mempty

data RecipientInfo =
     RecipientInfo 
     { riFirstName :: Text
     , riPhone     :: Text
     } deriving (Show, Eq, Generic)


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ri" } ''RequestInfo)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ps" } ''PlatformStation)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ld" } ''LocationDetails)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "cl" } ''CustomLocation)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "drn" } ''DestinationRequestNode)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "srn" } ''SourceRequestNode)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ibd" } ''ItemBillingDetails)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "bi" } ''BillingInfo)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "i" } ''Item)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pd" } ''PhysicalDimensions)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "p" } ''Place)
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ri" } ''RecipientInfo)

