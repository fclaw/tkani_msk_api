{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

import Text (recordLabelModifier, encodeToText)
import Infrastructure.Services.Types (PaymentProvider)
import Domain.Warehouse.Types (FabricType, Fabric (..))
import API.Types (RawIngestRequest (..), MediaType)
import Domain.Logic.Dimensions


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
    { nprOrderId            :: Text
    , nprProvider           :: PaymentProvider
    , nprProviderPaymentId  :: Text
    , nprAmountKopecks      :: Int64
    , nprPaymentUrl         :: Text
    , nprError              :: Maybe Text
    , nprToken              :: Text
    }


data DailyDigestStatus = Draft | Published | Ready

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''DailyDigestStatus)


data OrderItem = 
     OrderItem
     { -- | The human-readable name of the fabric being purchased (e.g., "Пальтовый кашемир от Dior").
       --   Source: Bot context, from the product the user initially selected. 
      oiName :: Text
       -- | The unique article number or SKU for the fabric in our internal system.
       --   This is crucial for SDEK fiscalization and our own database records.
       --   Source: Bot context, from the product the user initially selected.
     , oiArticle :: Text
       -- the part is required for the bank
     , oiFabricType    :: FabricType    -- The Fabric type
     , oiPricePerMetre :: Maybe Double  -- Price per meter for rolls
       -- | The final calculated price for the specific cut or piece of fabric.
       --   Source: Bot context, calculated based on length/pre-cut choice.
     , oiTotalPrice    :: Double        -- Total price for this line item
     , oiLengthM       :: Maybe Double  -- Length, only for rolls
     , oiTelegramUrl   :: Text
     }

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "oi" } ''OrderItem)


data PatchedFabric = 
     PatchedFabric
     { prId  :: Int64
     , prDescription :: Text
     , prLength :: Double
     , prWidth :: Int32
     , prPrice :: Int32
     , prIsSearchable :: Bool
     , prName :: Text
     , prFileId :: Maybe Text                 
     , prMediaGroupId :: Maybe Text   
     , prThumbnailUrl :: Maybe Text          
     , prMediaType :: Text
     , prGalleryDate :: Maybe Day
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
      prGalleryDate = rawGalleryDate
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
     { podSdekUuid      :: UUID
     , podParcelWeight  :: Int
     , podLength        :: Int
     , podWidth         :: Int
     , podHeight        :: Int
     , podItems :: [PatchedOrderDetailsItem]
     } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "pod" } ''PatchedOrderDetails)