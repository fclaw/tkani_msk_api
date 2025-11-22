{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Database.Types where


import           Data.Aeson -- You might want to derive To/FromJSON
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Control.Lens (makeLenses)
import           Data.UUID (UUID) -- For the SDEK tracking UUID
import           Data.Aeson.TH
import           Data.Int (Int64)

import Text (recordLabelModifier)


-- | Represents a complete Order in our system, mirroring the 'orders' DB table.
data Order = Order
  { -- | Primary key. The unique, human-friendly ID (e.g., "ORD-YYYYMMDD-XXXXXX").
    _orderId                          :: Text

    -- | Foreign key to the 'fabrics' table.
  , _orderFabricId                    :: Int64

    -- | Details of the fabric cut. Exactly one of these should be 'Just'.
  , _orderLengthM                     :: Maybe Double
  , _orderPreCutId                    :: Maybe Int64

    -- | Customer and delivery information gathered from the bot.
  , _orderCustomerFullName            :: Text
  , _orderCustomerPhone               :: Text
  , _orderDeliveryProviderId          :: Text
  , _orderDeliveryPointId             :: Text

    -- | A link back to the Telegram post advertising the fabric.
  , _orderTelegramUrl                 :: Text

    -- | The tracking UUID returned by SDEK's asynchronous registration.
    --   This is used by the polling worker.
  , _orderSdekRequestUuid             :: UUID

    -- | The permanent, official SDEK tracking number, received when registration is 'SUCCESSFUL'.
  , _orderSdekTrackingNumber          :: Text

    -- | The Telegram 'message_id' of the notification in the internal orders channel.
    --   Used to edit the message to update the status.
  , _orderInternalNotificationMessageId :: Int64
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
     } deriving (Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "af" } ''AdjustFabric)