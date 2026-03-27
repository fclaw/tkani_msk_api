-- We need a few language extensions for this.
{-# LANGUAGE DeriveGeneric              #-} -- To automatically derive Generic
{-# LANGUAGE DuplicateRecordFields      #-} -- To allow fields like 'priceRub' in multiple records
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API.Types where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), Value(..), withObject)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower)
import GHC.Generics (Generic)
import Data.Text (pack)

import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import Data.Aeson.TH
import Data.Int (Int64, Int32)
import Data.Time (Day, parseTimeM, defaultTimeLocale)

import API.WithField (WithField)
import Text (camelToSnake, recordLabelModifier) 
import Domain.Warehouse.Types (FabricType)
import Domain.Logic.Dimensions (FabricDensity)
import Domain.Warehouse.Enums (FabricLifecycle)



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


wrongModelErrorCode = "400" :: Text
wrongParamsErrorCode = "401" :: Text
cartLimitExceeded = "402" :: Text

mkError e = (ApiError mempty e)

-- | 1. Supported Media Types
data MediaType = 
       PHOTO 
     | VIDEO 
     | DOCUMENT
     | ANIMATION 
     | UNKNOWN 
     deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''MediaType)


data FabricProperties = 
     FabricProperties 
     { fpDensity        :: FabricDensity 
     , fpWeightPerMetre :: Double
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "fp" } ''FabricProperties)


-- | 2. The Ingest Payload (Matches your Python Dict keys exactly)
data RawIngestRequest = 
     RawIngestRequest
    { rawFabricId         :: Maybe Int64
    , rawText             :: Text        -- ^ The caption
    , rawMsgId            :: Int64       -- ^ Warehouse Message ID
    , rawMediaGroupId     :: Maybe Text  -- ^ Album ID (null/None if single)
    , rawMediaType        :: MediaType   -- ^ Parsed via the Enum above
    , rawFileId           :: Maybe Text  -- ^ The file ID
    , rawThumbnailUrl     :: Maybe Text
    , rawFabricProperties :: FabricProperties
    , rawLifeCycle        :: Maybe FabricLifecycle
    , rawDiscount         :: Maybe Double
    } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "raw" } ''RawIngestRequest)


data NewFabric = 
     NewFabric
     { nfId :: Int64
     , nfType :: FabricType
     , nfArticle :: Text
     }

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "nf" } ''NewFabric)

-- | Represents a specific, fixed-length pre-cut of a fabric.
--   This corresponds to the 'pre_cuts' table in the database.
data PreCut = PreCut
  { -- We use prefixes like 'pc' to avoid name collisions.
    pcLengthM :: Double  -- Corresponds to 'length_m'
  , pcPriceRub:: Int     -- Corresponds to 'price_rub' (in kopecks/cents)
  , pcInStock :: Bool    -- Corresponds to 'in_stock'
  } deriving (Show, Generic)


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pc" } ''PreCut)


data FabricPreviewStatus = 
       ItemInStock 
     | ItemSoldOut
     | ItemIsClaimed
     deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''FabricPreviewStatus)

-- | A unified, minimal preview for any sellable item (Roll or PreCut).
--   Provides just enough info for the bot to confirm the item and check stock.
data FabricPreview = FabricPreview
  { -- | The user-facing name for confirmation (e.g., "Шелк Армани" or "Отрез Шелк Армани 1.2м").
    fpName            :: Text
    -- | The price of one unit (EITHER per meter for a Roll, OR total for a PreCut).
  , fpPrice           :: Int
    -- | The available quantity (EITHER meters for a Roll, OR just 'True'/'False' for a PreCut).
    --   We can represent this as a Double for length, or 1.0/0.0 for PreCut availability.
  , fpStockAvailable  :: Double -- For a PreCut, this will be 1.0 if in stock, 0.0 if not.
  , fpStatus          :: FabricPreviewStatus
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "fp" } ''FabricPreview)


data Providers = SDEK | YANDEX | NONE
  deriving (Show, Eq, Read)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''Providers)

-- Convert from URL path segment TO our Providers type
instance FromHttpApiData Providers where
  parseUrlPiece text =
    -- We'll make it case-insensitive for robustness
    case T.toLower text of
      "sdek"     -> Right SDEK
      "yandex"   -> Right YANDEX
      _          -> Left "Unknown provider"

-- Convert from our Providers type TO a URL path segment
instance ToHttpApiData Providers where
  toUrlPiece provider =
    case provider of
      SDEK     -> "sdek"
      YANDEX   -> "yandex"

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
  { -- Customer & Delivery Information
    orTelegramUserId     :: Int64 
  , orCustomerFullName   :: Text       -- Full name as a single string (e.g., "Иванов Иван Иванович").
  , orCustomerPhone      :: Text       -- Phone number, normalized (e.g., "+79211234567").
  
    -- delivery provider info
  , orDeliveryProviderId :: Providers  -- The code for the provider, e.g., "sdek".
  , orDeliveryPointId    :: Text       -- The unique ID of the chosen delivery point.
  , orChatId             :: Int64
  , orIsPrepaid          :: Bool
  } deriving (Show, Generic)

-- We'll need ToJSON/FromJSON instances for this to be sent over the API
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "or" } ''OrderRequest)

-- | Represents the lifecycle stages of an order.
data OrderStatus
  = Registered          -- order is registered in a delivery provider
  | Paid                -- Payment received, awaiting fulfillment.
  | OnRoute             -- Courier has picked up the package, it's in transit.
  | Delivered           -- Customer has received the package.
  | Completed
  | Cancelled
  | PickedUpByCourier     -- the order has been picked up by the courier
  | ScheduledForPickup    -- the batch is scheduled for a pick-up
  | PickupFailed          -- the courier attempted pickup but failed 
  | AddedToPickupQueue    -- 
  deriving (Show, Eq, Ord, Read, Bounded, Enum, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''OrderStatus)

statusToSQL :: OrderStatus -> Text
statusToSQL s = case s of
    Registered         -> "registered"
    Paid               -> "paid"      -- Match your Postgres enum string EXACTLY
    OnRoute            -> "on_route"
    Delivered          -> "delivered"
    Completed          -> "completed"
    Cancelled          -> "cancelled"
    PickedUpByCourier  -> "picked_up_by_courier"
    ScheduledForPickup -> "scheduled_for_pickup"
    PickupFailed       -> "pickup_failed"
    AddedToPickupQueue -> "added_to_pickup_queue"
    -- etc...

-- | Converts an OrderStatus into a human-readable, formatted Russian Text
--   suitable for an internal notification channel.
formatStatus :: OrderStatus -> Text
formatStatus status = case status of
  -- After packing, the order has been successfully registered with SDEK (via API),
  -- and a tracking number has been generated. Ready for courier pickup.
  Registered         -> "📝 ЗАРЕГИСТРИРОВАН В СЛУЖБЕ ДОСТАВКИ"

  -- Payment is confirmed via Tinkoff webhook. Time to pick and pack.
  Paid               -> "✅ ОПЛАЧЕН, ГОТОВ К СБОРКЕ"

  -- The courier has scanned the package. It is now in transit.
  OnRoute            -> "🚚 В ПУТИ"

  -- SDEK reports that the package has arrived at the final delivery point.
  Delivered          -> "📦 ДОСТАВЛЕН В ПУНКТ ВЫДАЧИ"

  -- The customer has physically picked up the order. The transaction is fully complete.
  -- This status might be set manually or via another SDEK webhook.
  Completed          -> "🏁 ЗАВЕРШЁН (ВЫДАН КЛИЕНТУ)"
  
  -- The order was cancelled.
  Cancelled          -> "❌ ОТМЕНЁН"

  -- scheduled for pickup
  ScheduledForPickup -> "🗓️ ЗАПЛАНИРОВАН К ОТПРАВКЕ"

  -- The courier has picked up the package from our warehouse.
  PickedUpByCourier  -> "📬 ЗАБРАН КУРЬЕРОМ"

  -- The courier attempted to pick up the package but failed (e.g., wrong address, no one home).
  PickupFailed       -> "⚠️ НЕУДАЧНАЯ ПОПЫТКА ЗАБОРА ПАКЕТА"

  AddedToPickupQueue -> "🗓️ ОЖИДАЕТ ВКЛЮЧЕНИЯ В ПАРТИЮ"


-- A record to hold all the necessary information for the final confirmation.
data OrderConfirmationDetails = 
     OrderConfirmationDetails
     { orderId          :: Text -- e.g., "T-20231114-A4B7" - CRUCIAL for support
     , paymentLink      :: Text
     , trackingNumber   :: Maybe Text -- delivery provider tracking number
     , linkToQr         :: Maybe Text
     } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake } ''OrderConfirmationDetails)

-- | Request payload to link a sent Telegram message ID to an Order
data PaymentMessageDetailsRequest = 
     PaymentMessageDetailsRequest
     { pmdShelfOrderId :: Text  -- Matches "shelf_order_id"
     , pmdChatId       :: Int64   -- Matches "chat_id" (Must be Int64)
     , pmdMessageId    :: Int64   -- Matches "message_id"
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pmd" } ''PaymentMessageDetailsRequest)

data TrackOrder =
     TrackOrder 
     { toStatus         :: Text
     , toOrderId        :: Text
     , toTrackingNumber :: Maybe Text
     , toProvider       :: Providers
     } deriving (Show, Generic)
     
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "to" } ''TrackOrder)


newtype CatalogDate = CatalogDate Day

instance FromHttpApiData CatalogDate where
  parseUrlPiece dateStr = 
    -- parseTimeM is the standard way to parse time strings in Haskell.
    -- "%Y-%m-%d" is the format string for "Year-Month-Day".
    -- True - Accept leading/trailing whitespace?
    -- defaultTimeLocale - Use system's default locale for month names etc
    -- "%Y-%m-%d" - The format to expect
    let mRes = fmap CatalogDate $ parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateStr)
    in case mRes of Just v -> Right v; Nothing -> Left "wrong format"


data FabricMedia = 
     FabricMedia
     { fmTelegramFileId :: Text
     , fmMediaType      :: MediaType
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "fm" } ''FabricMedia)


-- | Represents a single item in the daily catalog carousel.
--   This is a "summary" DTO (Data Transfer Object).
data CatalogSummaryItem = CatalogSummaryItem
  { -- | The unique database ID for this fabric. Used to initiate a purchase.
    csiId                  :: Int64
    -- | The user-facing name of the fabric.
  , csiName                :: Text
    -- | The unique article/SKU.
  , csiArticle             :: Text
    -- | The type of product: a roll or a specific pre-cut.
  , csiType                :: FabricType -- e.g., "roll" or "pre_cut"
    -- | Price per meter. Only present for rolls.
  , csiPricePerMeter       :: Maybe Int
    -- | The total price for the piece. Only present for pre-cuts.
  , csiTotalPrice          :: Maybe Int
    -- | The length of the piece in meters. Only for pre-cuts.
  , csiLengthM             :: Maybe Double
    -- | The amount of fabric available for cut-to-order (in meters). Only for rolls.
  , csiAvailableLength     :: Maybe Double
    -- | Flag indicating if the item is sold out (roll has 0 length or pre-cut is sold).
  , csiIsSoldOut           :: Bool
    -- | The message_id of the ad post in the private warehouse channel.
  , csiWarehouseMessageId  :: Int64
    -- | The chat_id of the private warehouse channel.
  , csiWarehouseChatId     :: Int64
    -- | The file_id of the image thumbnail for this fabric.
  , csiWarehouseFileId     :: Maybe Text
    -- | The description text for this fabric.
  , csiDescription         :: Text
  , csiMediaType           :: MediaType
    -- | The **working width** of the fabric in centimeters.
    --   This is the usable width of the material, excluding the selvedges (кромки).
    --   It is the most critical measurement for tailoring and pattern layout, as selvedges
    --   are typically cut off and discarded.
  , csiWidth               :: Int
  , csiMediaList           :: [FabricMedia]
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "csi" } ''CatalogSummaryItem)


type CatalogSummaryItemExt = WithField "hash" (Maybe Int64) (WithField "discount" (Maybe Double) (CatalogSummaryItem))

-- | The top-level response for a catalog request.
data CatalogSummary = CatalogSummary
  { -- | The total number of items in a catalog.
    csTotalItems :: Int
    -- | The list of fabric summary items for the carousel.
  , csItems      :: [CatalogSummaryItemExt]
  } deriving (Show, Eq, Generic)


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "cs" } ''CatalogSummary)


-- | ADT for the lightweight search result list (Inline "Teaser" Mode)
-- Contains just enough info for the bot to display a title, description, and thumbnail.
data SearchTeaser = SearchTeaser
  { stId           :: Int64
  , stPreCutId     :: Maybe Int64
  , stName         :: Text
  , stArticle      :: Text
  , stType         :: FabricType
  , stPrice        :: Int       -- Can be price_per_meter or total_price
  , stDiscount     :: Double
  , stThumbnailUrl :: Maybe Text
  } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "st" } ''SearchTeaser)

-- NEW: The Paginated Response Wrapper
-- This is what your API will now return.
data PaginatedResults a = PaginatedResults
  { prItems      :: [a]   -- The list of items for the current page (e.g., [SearchTeaser])
  , prTotal      :: Int   -- The TOTAL number of items found across ALL pages
  , prPage       :: Int   -- The current page number
  , prLimit      :: Int   -- The number of items per page
  , prTotalPages :: Int   -- Calculated total pages (total / limit)
  } deriving (Show, Eq, Generic)


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pr" } ''PaginatedResults)

defPaginatedResults = PaginatedResults [] 0 0 0 0

data DailyDigestDraft =
     DailyDigestDraft 
     { dddChatId :: Int64
     , dddMessageId :: Int64
     , dddDraft :: Text
     }

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ddd" } ''DailyDigestDraft)

data DailyDigest =
     DailyDigest
     { ddChatId :: Int64
     , ddMessageId :: Int64
     }

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "dd" } ''DailyDigest)

data CancelOrder = CancelOrder { coOrderId :: Text }

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "co" } ''CancelOrder)


data CartCheckStatus = 
        CartExpired
      | ItemInCart 
      | OkToAdd 
      | NoCartExists
      | ItemIsAlreadyClaimed
  deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''CartCheckStatus)

data CheckItemInCart = CheckItemInCart { ciicCartStatus :: CartCheckStatus }

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ciic" } ''CheckItemInCart)

data CartNewFabric =
     CartNewFabric
     { cnfTelegramUserId :: Int64
     , cnfFabricId       :: Int64
     , cnfFabricLength   :: Maybe Double
     , cnfFabricType     :: FabricType
     , cnfTelegramUrl    :: Text
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "cnf" } ''CartNewFabric)

--          roll
--         "id": 123, // This is the cart_item_id, useful for editing
--         "name": "Шелк Армани",
--         "type": "roll",
--         "length_m": 1.5,
--         "price": 2250
--          precut
--          "id": 124,
--          "name": "Шерсть (отрез 2.0м)",
--          "type": "pre_cut",
--          "price": 2400
data ViewCartItem = 
     ViewCartItem
     { vciId :: Int64
     , vciName :: Text
     , vciType :: FabricType
     , vciLengthM :: Maybe Double
     , vciPrice :: Int
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "vci" } ''ViewCartItem)

data ViewCart =
     ViewCart
     { vcItemsCount :: Int 
     , vcTotalPrice :: Int
     , vcItems :: [ViewCartItem]
     }  deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "vc" } ''ViewCart)


data MeasureRequest =
     MeasureRequest
     { mrTrackingNumber :: Text
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "mr" } ''MeasureRequest)

data MeasureResponse =
     MeasureResponse
     { mresIsMeasured :: Bool
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "mres" } ''MeasureResponse)


data YamlOrderItem =
      YamlOrderItem
     { yoiName          :: Text
     , yoiFabricType    :: FabricType  
     , yoiPricePerMetre :: Maybe Double 
     , yoiTotalPrice    :: Double
     , yoiLengthM       :: Maybe Double
     , yoiWeight        :: Int -- in grams
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "yoi" } ''YamlOrderItem)

data PhysicalDimensions =
     PhysicalDimensions
     { pdWidth  :: Int
     , pdLength :: Int
     , pdHeight :: Int
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pd" } ''PhysicalDimensions)

data YamlOrderRequest =
     YamlOrderRequest
     { yorCustomerFullName :: Text
     , yorCustomerPhone :: Text
     , yorDeliveryPointId :: Text
     , yorDeliveryProviderId :: Providers
     , yorItems :: [YamlOrderItem]
     , yorPhysicalDimensions :: PhysicalDimensions
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "yor" } ''YamlOrderRequest)

data YamlOrderResponse =
     YamlOrderResponse
     { yorOrderId :: Text
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "yor" } ''YamlOrderResponse)

data Expenses =
     Expenses
     { expAmount      :: Double
     , expPayer       :: Maybe Text -- nothing stands for company flag
     , expDescription :: Maybe Text
     , expDay         :: Maybe Day -- defaults to now
     } deriving (Show, Eq)
     
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "exp" } ''Expenses)

data SetOrderDimensionsRequest =
     SetOrderDimensionsRequest
     { sodrLength  :: Int
     , sodrWidth   :: Int
     , sodrHeight  :: Int
     , sodrWeight  :: Maybe Int
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sodr" } ''SetOrderDimensionsRequest)

data SdekDeliveryPoint =
     SdekDeliveryPoint
     { spdCode    :: Text
     , spdAddress :: Text
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "spd" } ''SdekDeliveryPoint)

data ShelfRequest =
     ShelfRequest
     { srInitials :: Text
     , srPhone    :: Text
     , srRegion   :: Text
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sr" } ''ShelfRequest)

data ShelfIdResponseStatus = Ok | Already | CapacityExceeded
  deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''ShelfIdResponseStatus)

data ShelfIdResponse = 
     ShelfIdResponse 
     { sirId     :: Maybe Int64
     , sirStatus :: ShelfIdResponseStatus
     } 
     deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sir" } ''ShelfIdResponse)

data ShelfItems =
     ShelfItems
     { siArticle    :: Text
     , siName       :: Text
     , siFabricType :: FabricType
     , siQuantity   :: Double
     , siPrice      :: Int
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "si" } ''ShelfItems)


data ShelfItemsResponse =
     ShelfItemsResponse
     { sirCapacity       :: Int32
     , sirItems          :: [ShelfItems]
     , sirLifeTimeInDays :: Maybe Int
     } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sir" } ''ShelfItemsResponse)



data PutOnShelfRequest =
     PutOnShelfRequest
     { posrChatId   :: Int64
     } deriving (Show, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "posr" } ''PutOnShelfRequest)


data ShelfStatus = Requested | Waitlisted | Active | Absent
  deriving (Show, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''ShelfStatus)

data PutOnShelfPaymentOptions = 
     PutOnShelfPaymentOptions
     { pspoPaymentLink      :: Maybe Text
     , pspoTotalPrice       :: Maybe Double
     , pspoLinkToQr         :: Maybe Text
     , pspoOrderId          :: Maybe Text
     , pspoShelfStatus      :: ShelfStatus
    } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pspo" } ''PutOnShelfPaymentOptions)


mkDefPutOnShelfPaymentOptions = PutOnShelfPaymentOptions Nothing Nothing Nothing Nothing Requested

data InitiateShelfShipment =
     InitiateShelfShipment
     { issProvider  :: Providers
     , issPointId   :: Text
     , issIsPrepaid :: Bool
     } deriving (Show, Generic)
      

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "iss" } ''InitiateShelfShipment)

data ShelfShipmentDetails =
     ShelfShipmentDetails
     { ssdOrderId          :: Text
      -- | The delivery tracking number provided by the delivery service.
     , ssdTrackingNumber   :: Maybe Text
     , ssdDeliveryProvider :: Providers
     } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ssd" } ''ShelfShipmentDetails)


newtype ShelfStatusResponse = ShelfStatusResponse { shelf_status :: ShelfStatus }
  deriving (Show, Generic, ToJSON)

data ShelfSubmissionChatDetails =
      ShelfSubmissionChatDetails
      { sscdUserId    :: Int64
      , sscdChatId    :: Int64
      , sscdMessageId :: Int64
      } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sscd" } ''ShelfSubmissionChatDetails)


data ShelfPersonalInfo =
     ShelfPersonalInfo
     { spiFullName           :: Maybe Text
     , spiPhone              :: Maybe Text
     , spiPreferredSdekPoint :: Maybe Text
     } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "spi" } ''ShelfPersonalInfo)


data FabricMediaRequest = 
     FabricMediaRequest
     { fmrFabricId       :: Int64
     , fmrFabricType     :: FabricType
     , fmrTelegramFileId :: Text
     , fmrMediaType      :: MediaType
     } deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "fmr" } ''FabricMediaRequest)

data PreferredSdekPointWithAddress =
     PreferredSdekPointWithAddress
     { pswaCode    :: Text
     , pswaAddress :: Text
     } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pswa" } ''PreferredSdekPointWithAddress)

data YandexDeliveryCity = 
     YandexDeliveryCity
     { ydcCode    :: Int
     , ydcName    :: Text
     } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ydc" } ''YandexDeliveryCity)

data YandexPickupPointsResp = 
     YandexPickupPointsResp
     { ypprTotal  :: Int
     , ypprPoints :: [WithField "isPrepaid" Bool (WithField "dpMetros" [Text] DeliveryPoint)]
     } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "yppr" } ''YandexPickupPointsResp)

data YandexShipmentFinalizeReq = 
      YandexShipmentFinalizeReq 
      { ysfrOrderId    :: Text
      , ysfrUserId     :: Int64
      , ysfrChatId     :: Int64
      , ysfrWaitMsgId  :: Int64
      } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "ysfr" } ''YandexShipmentFinalizeReq)