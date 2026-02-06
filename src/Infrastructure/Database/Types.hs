{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Infrastructure.Database.Types where


import           Data.Aeson -- You might want to derive To/FromJSON
import           Data.Text (Text)
import           Data.Time (UTCTime, Day)
import           GHC.Generics (Generic)
import           Control.Lens (makeLenses)
import           Data.UUID (UUID) -- For the SDEK tracking UUID
import           Data.Aeson.TH
import           Data.Int (Int64, Int32)
import           Data.Char (toLower)
import           Data.Time.Calendar.Month (Month)


import Domain.Logic.Dimensions
import Domain.Warehouse.Enums (FabricLifecycle)
import Text (recordLabelModifier, encodeToText)
import Infrastructure.Services.Types (PaymentProvider)
import Domain.Warehouse.Types (FabricType, Fabric (..))
import API.Types (RawIngestRequest (..), MediaType)


-- | Represents a complete Order in our system, mirroring the 'orders' DB table.
data Order = Order
  { -- | Primary key. The unique, human-friendly ID (e.g., "ORD-YYYYMMDD-XXXXXX").
    _orderId                            :: Text

    -- | Customer and delivery information gathered from the bot.
  , _orderCustomerFullName              :: Text
  , _orderCustomerPhone                 :: Text
  , _orderDeliveryProviderId            :: Text
  , _orderDeliveryPointId               :: Text

    -- | The tracking UUID returned by SDEK's asynchronous registration.
    --   This is used by the polling worker.
  , _orderSdekRequestUuid               :: UUID

    -- | The permanent, official SDEK tracking number, received when registration is 'SUCCESSFUL'.
  , _orderSdekTrackingNumber            :: Text

    -- | The Telegram 'message_id' of the notification in the internal orders channel.
    --   Used to edit the message to update the status.
  , _orderInternalNotificationMessageId :: Int64

  , _orderTelegramUserId                :: Int64
  , _orderTariff                        :: Int32
  } deriving (Show, Eq, Generic)

-- This Template Haskell splice automatically generates lenses for each field.
-- e.g., 'orderId' will be a lens for the '_orderId' field.
makeLenses ''Order

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "_order" } ''Order)

data AdjustFabric = 
     AdjustFabric
     { afName :: Text
     , afArticle :: Text
     , afIsSold :: Bool
     , afIsPreCutReq :: Bool
     , afRemLength :: Double
     , afWarehouseMessageId :: Int64
     } deriving (Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "af" } ''AdjustFabric)


data NewPaymentRecord = 
    NewPaymentRecord
    { nprOrderId            :: Maybe Text
    , nprProvider           :: PaymentProvider
    , nprProviderPaymentId  :: Text
    , nprAmountKopecks      :: Int64
    , nprPaymentUrl         :: Text
    , nprError              :: Maybe Text
    , nprToken              :: Text
    , nprPaymentFlow        :: Text
    , nprShelfOrderId       :: Maybe Text
    }


data DailyDigestStatus = Draft | Published | Ready

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''DailyDigestStatus)


data OrderItem = 
     OrderItem
     { -- | The human-readable name of the fabric being purchased (e.g., "Пальтовый кашемир от Dior").
       --   Source: Bot context, from the product the user initially selected. 
      oiName           :: Text
       -- | The unique article number or SKU for the fabric in our internal system.
       --   This is crucial for SDEK fiscalization and our own database records.
       --   Source: Bot context, from the product the user initially selected.
     , oiArticle       :: Text
       -- the part is required for the bank
     , oiFabricType    :: FabricType    -- The Fabric type
     , oiPricePerMetre :: Maybe Double  -- Price per meter for rolls
       -- | The final calculated price for the specific cut or piece of fabric.
       --   Source: Bot context, calculated based on length/pre-cut choice.
     , oiTotalPrice    :: Double        -- Total price for this line item
     , oiLengthM       :: Maybe Double  -- Length, only for rolls
     , oiTelegramUrl   :: Text
     , oiThumbnailUrl  :: Maybe Text
     } deriving (Show, Eq, Generic)


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "oi" } ''OrderItem)

data PutOnShelfDetails =
     PutOnShelfDetails
     { posdShelfId           :: Int64
     , posdUserInitials      :: Text
     , posdPhone             :: Text
     , posdItems             :: [OrderItem]
     , posdItemsOnShelfCount :: Int32
     }

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "posd" } ''PutOnShelfDetails)

data PatchedFabric = 
     PatchedFabric
     { prId              :: Int64
     , prDescription     :: Text
     , prLength          :: Double
     , prWidth           :: Int32
     , prPrice           :: Int32
     , prIsSearchable    :: Bool
     , prName            :: Text
     , prFileId          :: Maybe Text                 
     , prMediaGroupId    :: Maybe Text   
     , prThumbnailUrl    :: Maybe Text          
     , prMediaType       :: Text
     , prLifeCycle       :: Maybe Text
     , prDiscount        :: Maybe Double
     } deriving (Show, Eq, Generic)


mkPatchedFabric :: Int64 -> Fabric -> RawIngestRequest -> PatchedFabric
mkPatchedFabric fabricId Fabric {..} RawIngestRequest {..} =
  let prId = fabricId
      prDescription = fDescription
      prLength = fLength
      prWidth = fromIntegral fWidth
      prPrice = fromIntegral fPrice
      prIsSearchable = fIsSearchable
      prName = fName
      prFileId = rawFileId
      prMediaGroupId = rawMediaGroupId
      prThumbnailUrl = rawThumbnailUrl
      prMediaType = encodeToText rawMediaType
      prLifeCycle = fmap encodeToText rawLifeCycle
      prDiscount = rawDiscount
  in PatchedFabric {..}


data PriceInfo =
     PriceInfo 
     { piTariff      :: Int
     , piPickUpPoint :: Text
     , piWeight      :: Int  {- Total weight (in grams) -}
     , piLength      :: Int
     , piWidth       :: Int
     , piHeight      :: Int
     , piPrice       :: Int
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pi" } ''PriceInfo)

defPriceInfo = PriceInfo 0 mempty 0 0 0 0

data PriceInfoBotItem =
     PriceInfoBotItem
     { pibiDensity        :: FabricDensity
     , pibiWidth          :: Double
     , pibiLength         :: Double
     , pibiWeightPerMetre :: Double
     , pibiPrice          :: Int
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pibi" } ''PriceInfoBotItem)

data PriceInfoBot =
     PriceInfoBot 
     { pibTariff :: Int
     , pibPickUpPoint :: Text
     , pibItems :: [PriceInfoBotItem] 
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pib" } ''PriceInfoBot)

reducePriceInfoBot :: PriceInfoBot -> PriceInfo
reducePriceInfoBot PriceInfoBot {..} =
  let getTotalWeight acc [] = acc
      getTotalWeight old (PriceInfoBotItem {..} : xs) =
        let new = estimatePackedWeight old pibiWeightPerMetre pibiLength
        in getTotalWeight new xs
      (lengthXs, widthXs, heightXs) = 
        unzip3 [ estimatePackedDimensions 
                 pibiDensity 
                 pibiWidth 
                 pibiLength 
               | PriceInfoBotItem {..} <- pibItems ] 
      piTariff      = pibTariff
      piPickUpPoint = pibPickUpPoint
      piWeight      = getTotalWeight packagingWeightGrams pibItems
      piLength      = maximum lengthXs
      piWidth       = maximum widthXs
      piHeight      = sum heightXs
      piPrice       = sum (map pibiPrice pibItems)
  in PriceInfo {..}

data  YamlOrder =
      YamlOrder 
      { _yamlOrderId                  :: Text
      , _yamlOrderCustomerFullName    :: Text
      , _yamlOrderCustomerPhone       :: Text
      , _yamlOrderDeliveryProviderId  :: Text
      , _yamlOrderDeliveryPointId     :: Text
      , _yamlOrderSdekRequestUuid     :: UUID
      , _yamlOrderSdekTrackingNumber  :: Text
      , _yamlOrderTariff              :: Int32
      , _yamlOrderWeight              :: Int32
      , _yamlOrderLength              :: Int32
      , _yamlOrderWidth               :: Int32
      , _yamlOrderHeight              :: Int32
      } deriving (Show, Eq, Generic)

makeLenses ''YamlOrder

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "_yamlOrder" } ''YamlOrder)


data PatchedOrderDetailsItem = 
     PatchedOrderDetailsItem 
     { podiName :: Text
     , podiArticle :: Text
     , podiWeight :: Int
     , podiCost :: Double
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "podi" } ''PatchedOrderDetailsItem)


data PatchedOrderDetails =
     PatchedOrderDetails
     { podSdekUuid :: UUID
     , podItems    :: [PatchedOrderDetailsItem]
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pod" } ''PatchedOrderDetails)

data OrderDeliveryItem = 
     OrderDeliveryItem
     { odiId            :: Text 
     , odiTrack         :: Text
     , odiKeepFreeUntil :: Maybe UTCTime 
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "odi" } ''OrderDeliveryItem)

data DailyExpensesStat =
     DailyExpensesStat
     { desPayer        :: Text
     , desAmount       :: Double
     , desTransactions :: Int
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "des" } ''DailyExpensesStat)

-- Your ADT
data MonthlyStat = 
     MonthlyStat
     { msSaleMonth       :: Month
     , msTotalOrders     :: Int32
     , msAvgOrdersPerDay :: Int32
     , msTotalProfit     :: Double
     , msAvgProfitPerDay :: Double
     , msTotalExpenses   :: Double
     , msPayersExpenses  :: Either Text [DailyExpensesStat]
     } deriving (Show)


data CourierService = DOSTAVISTA | NONE deriving Show

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''CourierService)


data CourierPickupData = 
     CourierPickupData
     { cpdDay                   :: Day
     , cpdProvider              :: CourierService
     , cpdOrders                :: [Text]
     , cpdDostavistaOrderId     :: Int64
     , cpdDostavistaOrderStatus :: Text
     , cpdCost                  :: Double
     , cpdOrderStatus           :: Text
     } deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "cpd" } ''CourierPickupData)



data SpecialPostDetailsItems =
     SpecialPostDetailsItems
     { name      :: Text
     , discount  :: Int32
     } deriving (Show) 

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier mempty } ''SpecialPostDetailsItems)


-- message_id, posted_at, items_count, list of ALL names, list of 9 random thumbnails
data SpecialPostDetails = 
     SpecialPostDetails
     { messageId           :: Maybe Int64
     , postedAt            :: Maybe UTCTime
     , itemsCount          :: Int32
     , items               :: [SpecialPostDetailsItems]
     , randomThumbnailUrls :: [Text]
     } deriving (Show)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier mempty } ''SpecialPostDetails)


data ShelfOderStatus = Registered | Paid | Cancelled

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''ShelfOderStatus)


data ShelfItemsForShipment = 
     ShelfItemsForShipment
     { sifsShelfId      :: Int64
     , sifsUserInitials :: Text
     , sifsPhone        :: Text
     , sifsItems        :: [OrderItem]
     }

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sifs" } ''ShelfItemsForShipment)