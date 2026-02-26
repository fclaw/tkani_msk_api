{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Workers.ShelfOrderRegister.Order (place) where


import Katip
import Data.Text (Text)
import Data.Int (Int64)
import Data.UUID (UUID)
import Data.Coerce (coerce)
import Control.Monad (void, join)
import Data.Traversable (for)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Bifunctor (first, second)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Time (formatTime, defaultTimeLocale, LocalTime)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)

 
import Text (tshow, encodeToText)
import TH.Location (currentModule)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Utils.OrderId (generateOrderId)
import App (AppM, _appDBPool, _sdekConfig, Config, currentTime, render, ChatKey (SHELF))
import Workers.SimpleOrderOrchestrator.Order (fetchOrderPollerRes, PlaceOrderError (..), formatOrderItemLine)
import qualified Infrastructure.Services.Sdek as Sdek
import qualified Infrastructure.Services.Sdek.Types as Sdek
import qualified Infrastructure.Services.Sdek.Types.Config as Sdek
import Infrastructure.Services.Sdek.CachedTariffs (getTariffs)
import API.Types (ShelfShipmentDetails (..), InitiateShelfShipment (..))
import Infrastructure.Database (fetchShelfItemsForShipment, placeNewShelfOrder, ShelfItemsForShipment (..), Order (..))
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, deleteMessage, MessageIdResponse (..))


wrap action error = withExceptT error (ExceptT action)

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
go userId cfg init@InitiateShelfShipment {..} shipment@ShelfItemsForShipment {..} = do
  let sdekConfig = _sdekConfig cfg
  let shipmentPoint = Sdek.dropOffPoint sdekConfig
  let tariffCodes = Sdek.tariffs sdekConfig


  (uuid, optimalTariff) <- tryTariffs init shipment shipmentPoint tariffCodes


  ePollerRes <- fetchOrderPollerRes uuid
  trackingNumber <- except $ (first SdekPollerError) ePollerRes
  ssdOrderId <- liftIO generateOrderId

  telegramMsgId <- wrapOrCancel (notifyShelfChannel shipment ssdOrderId) NotificationSendFailed $ void (Sdek.cancelOrder uuid)
  let dbOrder = mkDbOrder userId init shipment uuid trackingNumber ssdOrderId telegramMsgId optimalTariff
  pool <- fmap _appDBPool $ lift ask
  let clearArtifacts = do 
        void $ Sdek.cancelOrder uuid
        void $ deleteMessage (coerce telegramMsgId) SHELF
  void $ wrapOrCancel (placeNewShelfOrder dbOrder pool) DatabaseFailed clearArtifacts

  let ssdTrackingNumber = trackingNumber
  let ssdDeliveryProvider = issProvider
  return ShelfShipmentDetails {..}


tryTariffs :: InitiateShelfShipment -> ShelfItemsForShipment -> Text -> [Sdek.Tariff] -> ExceptT PlaceOrderError AppM (UUID, Int)
tryTariffs InitiateShelfShipment {..} ShelfItemsForShipment {..} shipmentPoint tariffs = do 
  maybeSdekRes <- wrap(getTariffs shipmentPoint issPointId) TariffNetworkError
  let eSdekRes = maybe (Left "getTariffs:empty list") Right maybeSdekRes
  availableTariffs <- except $ (first TariffError) eSdekRes
  let optimalTariff = Sdek.findOptimalTariff tariffs availableTariffs
  let minOderReq = Sdek.makeMinimalShelfRequestData sifsUserInitials sifsPhone issPointId optimalTariff sifsItems (Just shipmentPoint)
  wrap (fmap (second (,optimalTariff)) (Sdek.registerOrder (Sdek.buildMinimalOderRequest minOderReq))) SdekRegistrationFailed


notifyShelfChannel :: ShelfItemsForShipment -> Text -> AppM (Either Text MessageIdResponse)
notifyShelfChannel shipment orderId = do
  tm <- currentTime
  tz <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime tz tm
  messageText <- render $currentModule $ buildTemplateData orderId shipment localTime
  fmap (first (T.pack . show)) $ sendOrEditTelegramMessage mempty (escapeMarkdownV2 messageText) SHELF Nothing Nothing Nothing

buildTemplateData :: Text -> ShelfItemsForShipment -> LocalTime -> HM.HashMap Text Text
buildTemplateData orderId ShelfItemsForShipment {..} localTime =
  let
    -- 1. Format common values
    timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" localTime
    itemCount = T.pack $ show $ length sifsItems

    -- 2. Build the 'itemsBlock' by mapping over the list
    itemLines = map formatOrderItemLine sifsItems
    itemsBlock = T.unlines itemLines
    
  in
    -- 3. Construct the final HashMap
    HM.fromList
      [ ("orderId", orderId)
      , ("shelfId", tshow sifsShelfId)
      , ("timestamp", timeStr)
      , ("customerName", sifsUserInitials)
      , ("customerPhone", sifsPhone)
      
      -- NEW: Variables for the item list
      , ("itemCount", itemCount)
      , ("itemsBlock", itemsBlock)
      ]

mkDbOrder :: Int64 -> InitiateShelfShipment -> ShelfItemsForShipment -> UUID -> Text -> Text -> MessageIdResponse -> Int -> Order
mkDbOrder userId InitiateShelfShipment {..} ShelfItemsForShipment {..} uuid trackingNumber orderId telegramMsgId tariff =
  Order 
  { _orderTariff                        = fromIntegral tariff
  , _orderId                            = orderId
  , _orderCustomerFullName              = sifsUserInitials
  , _orderCustomerPhone                 = sifsPhone
  , _orderDeliveryProviderId            = encodeToText issProvider
  , _orderDeliveryPointId               = issPointId
  , _orderSdekRequestUuid               = uuid
  , _orderSdekTrackingNumber            = trackingNumber
  , _orderInternalNotificationMessageId = coerce telegramMsgId
  , _orderTelegramUserId                = userId
  }