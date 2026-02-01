{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module API.Handlers.Shelf.InitiateShipment (handler) where


import Katip
import Data.Text (Text)
import Data.Int (Int64)
import Data.UUID (UUID)
import Data.Coerce (coerce)
import Control.Monad (void)
import Data.Foldable (for_)
import qualified Data.Text as T
import Data.Bifunctor (first)
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
import API.Handlers.PlaceNewOrder(fetchOrderPollerRes, PlaceOrderError (..), formatOrderItemLine)
import qualified Infrastructure.Services.Sdek as Sdek
import qualified Infrastructure.Services.Sdek.Types as Sdek
import qualified Infrastructure.Services.Sdek.Types.Config as Sdek
import API.Types (ApiResponse, ShelfShipmentDetails (..), InitiateShelfShipment (..), mkError)
import Infrastructure.Database (fetchShelfItemsForShipment, placeNewShelfOrder, ShelfItemsForShipment (..), Order (..))
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, MessageIdResponse (..))


wrap action error = withExceptT error (ExceptT action)


handler :: Int64 -> InitiateShelfShipment -> AppM (ApiResponse ShelfShipmentDetails)
handler userId init = do
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- fetchShelfItemsForShipment userId pool
  case eDbRes of 
    Left err ->
      fmap (const (Left (mkError "server error"))) $ 
        $(logTM) ErrorS $ "db failure " <> ls (tshow (err))
    Right Nothing -> pure $ Left $ mkError "you have no items to be shipped"
    Right (Just shipment) -> fmap (first (const (mkError "server error"))) $ runExceptT $ registerOrder userId cfg init shipment


registerOrder :: Int64 -> Config -> InitiateShelfShipment -> ShelfItemsForShipment ->  ExceptT PlaceOrderError AppM ShelfShipmentDetails
registerOrder userId cfg init@InitiateShelfShipment {..} shipment@ShelfItemsForShipment {..} = do
  let sdekConfig = _sdekConfig cfg
  let senderLocation = Sdek.senderLocation sdekConfig
  let fromLocation =
        Sdek.defSdekFromLocation
        { Sdek.sflAddress = Sdek.address senderLocation
        , Sdek.sflCode = Sdek.cityCode senderLocation
        , Sdek.sflPostCode = Just $ Sdek.postalCode senderLocation
        }
  let minOderReq = Sdek.makeMinimalShelfRequestData sifsUserInitials sifsPhone issPointId 136 sifsItems fromLocation
  uuid <- wrap(Sdek.registerOrder $ Sdek.buildMinimalOderRequest minOderReq) SdekRegistrationFailed
  ePollerRes <- fetchOrderPollerRes uuid
  trackingNumber <- except $ (first SdekPollerError) ePollerRes
  ssdOrderId <- liftIO generateOrderId

  telegramMsgId <- wrap (notifyShelfChannel shipment ssdOrderId) NotificationSendFailed
  let dbOrder = mkDbOrder userId init shipment uuid trackingNumber ssdOrderId telegramMsgId
  pool <- fmap _appDBPool $ lift ask
  void $ wrap (placeNewShelfOrder dbOrder pool) $ DatabaseFailed

  let ssdTrackingNumber = trackingNumber
  let ssdDeliveryProvider = issProvider
  return ShelfShipmentDetails {..}


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

mkDbOrder :: Int64 -> InitiateShelfShipment -> ShelfItemsForShipment -> UUID -> Text -> Text -> MessageIdResponse -> Order
mkDbOrder userId InitiateShelfShipment {..} ShelfItemsForShipment {..} uuid trackingNumber orderId telegramMsgId =
  Order 
  { _orderTariff                        = 136
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