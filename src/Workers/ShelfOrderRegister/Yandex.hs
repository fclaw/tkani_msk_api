{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Workers.ShelfOrderRegister.Yandex (place) where

import Data.Text (Text)
import Data.Int (Int64)
import Control.Monad (join, void)
import Data.Coerce (coerce)
import Data.Aeson (toJSON)
import Data.Functor ((<&>))
import Data.Traversable (for)
import Data.Bifunctor (first)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Monad.IO.Class (liftIO)

import Text (tshow)
import App (AppM, Config, _appDBPool, ChatKey (SHELF), _yandexConfig, _yandexWarehouseId, readTVarIO)
import Katip (logTM, Severity (..), ls)
import Infrastructure.Utils.OrderId (generateOrderId)
import Domain.Services.Warehouse (ensureWarehousePlatformId)
import API.Types (ShelfShipmentDetails (..), InitiateShelfShipment (..))
import  Workers.SimpleOrderOrchestrator.Yandex (PlaceOrderError (..))
import Workers.ShelfOrderRegister.Sdek (notifyShelfChannel, mkDbOrder)
import Infrastructure.Services.Yandex.Types.Enums (Tariff (SelfPickup))
import Infrastructure.Services.Yandex.Types (YandexCreateOrderReq (..), platformStationId)
import Infrastructure.Services.Yandex.Order
import Infrastructure.Database (fetchShelfItemsForShipment, placeNewShelfOrder, OrderItem (..), ShelfItemsForShipment (..), _orderYandex, YandexOrder (..))
import Infrastructure.Services.Telegram (deleteMessage, MessageIdResponse (..))


wrap action error = withExceptT error (ExceptT action)
{-# INLINE wrap #-}

wrapOrCancel :: AppM (Either e a) -> (e -> PlaceOrderError) -> AppM () -> ExceptT PlaceOrderError AppM a
wrapOrCancel action errorWrapper cleanup = wrap action errorWrapper `catchE` \err -> lift cleanup >> throwE err
{-# INLINE wrapOrCancel #-}

place :: Int64 -> InitiateShelfShipment -> AppM (Either Text (Maybe ShelfShipmentDetails))
place userId init = do
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- fetchShelfItemsForShipment userId pool
  fmap join $ for eDbRes $ \maybeShipment -> 
    case maybeShipment of 
      Nothing -> pure $ Right Nothing
      Just shipment -> do
        eRes <- runExceptT $ go userId cfg init shipment
        fmap (first tshow) $ for eRes $ \details -> pure $ Just details 


go :: Int64 -> Config -> InitiateShelfShipment -> ShelfItemsForShipment -> ExceptT PlaceOrderError AppM ShelfShipmentDetails
go userId cfg init shipment = do
  ssdOrderId <- liftIO generateOrderId
  telegramMsgId <- wrap (notifyShelfChannel shipment ssdOrderId) NotificationSendFailed
  cfg <- lift ask
  -- drop off point (platform station id)
  stateVar <- lift get
  maybeWarehouseId <- fmap _yandexWarehouseId $ lift $ readTVarIO stateVar
  let eWarehouseId =
        case maybeWarehouseId of
         Nothing            -> Left WarehouseNotSet
         Just warehouseId   -> Right $ platformStationId warehouseId
  sourcePointId <- except eWarehouseId
  let yaOrder = mkYaOrder ssdOrderId sourcePointId (issPointId init) shipment
  let dbOrder = mkDbOrder userId init shipment ssdOrderId telegramMsgId
  pool <- fmap _appDBPool $ lift ask
  let clearArtifacts = void $ deleteMessage (coerce telegramMsgId) SHELF
  void $ wrapOrCancel (placeNewShelfOrder (dbOrder { _orderYandex = Just yaOrder}) pool) DatabaseFailed clearArtifacts

  let ssdTrackingNumber = Nothing
  let ssdDeliveryProvider = issProvider init
  return ShelfShipmentDetails {..}


mkYaOrder :: Text -> Text -> Text -> ShelfItemsForShipment -> YandexOrder
mkYaOrder orderId sourcePointId destPointId ShelfItemsForShipment {..} =
  let draftJson = toJSON $
        YandexCreateOrderReq
        { info         = defRequestInfo { riOperatorRequestId = orderId }
         , source      = SourceRequestNode (PlatformStation sourcePointId)
         , destination = defDestinationRequestNode { drnPlatformStation = Just (PlatformStation destPointId) }
         , billingInfo = defBillingInfo
         , items       = 
             sifsItems <&> \OrderItem {..} -> 
               Item 
               { iCount          = 1
               , iName           = oiName
               , iArticle        = oiArticle
               , iBillingDetails =
                  ItemBillingDetails 
                  { ibdUnitPrice         = round oiTotalPrice
                  , ibdAssessedUnitPrice = round oiTotalPrice
                  }
               , iPlaceBarcode   = orderId
               }
         , places         = [defPlace]
         , recipientInfo  = RecipientInfo sifsUserInitials sifsPhone
         , lastMilePolicy = SelfPickup
         }
  in
    YandexOrder
    { yaDeliveryPoint  = destPointId
    , yaTariff         = SelfPickup
    , yaDraftJson      = draftJson
    }