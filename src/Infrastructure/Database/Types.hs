{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Infrastructure.Database.Types where


import           Data.Aeson -- You might want to derive To/FromJSON
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Control.Lens (makeLenses)
import           Data.UUID (UUID) -- For the SDEK tracking UUID
import           Data.Aeson.TH
import           Data.Int (Int64)
import           Data.Char (toLower)

import Text (recordLabelModifier, encodeToText)
import Infrastructure.Services.Types (PaymentProvider)
import Domain.Warehouse.Types (FabricType, Fabric (..))
import API.Types (RawIngestRequest (..), MediaType)



-- | Represents a complete Order in our system, mirroring the 'orders' DB table.
data Order = Order
  { -- | Primary key. The unique, human-friendly ID (e.g., "ORD-YYYYMMDD-XXXXXX").
    _orderId                          :: Text

    -- | Customer and delivery information gathered from the bot.
  , _orderCustomerFullName            :: Text
  , _orderCustomerPhone               :: Text
  , _orderDeliveryProviderId          :: Text
  , _orderDeliveryPointId             :: Text

    -- | The tracking UUID returned by SDEK's asynchronous registration.
    --   This is used by the polling worker.
  , _orderSdekRequestUuid             :: UUID

    -- | The permanent, official SDEK tracking number, received when registration is 'SUCCESSFUL'.
  , _orderSdekTrackingNumber          :: Text

    -- | The Telegram 'message_id' of the notification in the internal orders channel.
    --   Used to edit the message to update the status.
  , _orderInternalNotificationMessageId :: Int64

  , _orderTelegramUserId                :: Int64
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
     , afWarehouseMessageId :: Int
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
     , prWidth :: Int
     , prPrice :: Int
     , prIsSearchable :: Bool
     , prName :: Text
     , prFileId :: Maybe Text                 
     , prMediaGroupId :: Maybe Text   
     , prThumbnailUrl :: Maybe Text          
     , prMediaType :: Text
     }

mkPatchedFabric :: Int64 -> Fabric -> RawIngestRequest -> PatchedFabric
mkPatchedFabric fabricId Fabric {..} RawIngestRequest {..} =
  let prId = fabricId
      prDescription = fDescription
      prLength = fLength
      prWidth = fWidth
      prPrice = fPrice
      prIsSearchable = fIsSearchable
      prName = fName
      prFileId = rawFileId
      prMediaGroupId = rawMediaGroupId
      prThumbnailUrl = rawThumbnailUrl
      prMediaType = encodeToText rawMediaType
  in PatchedFabric {..}