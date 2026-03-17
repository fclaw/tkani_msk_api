{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Workers.SimpleOrderOrchestrator.Yandex (place, PlaceOrderError (..)) where


import Katip (logTM, Severity (..), ls)
import Data.Text (Text)
import Data.Int (Int64, Int32)
import Data.Aeson (toJSON, Value)
import qualified Data.Text as T
import Control.Monad (void, when)
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Control.Monad.Trans.Except
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)

import App
import Text (encodeToText, tshow)
import Infrastructure.Utils.OrderId (generateOrderId)
import qualified Infrastructure.Services.Tinkoff as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Security as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Init as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.GetState as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.QR as Tinkoff
import qualified Infrastructure.Services.Tinkoff.Types.Enum as Tinkoff
import qualified Workers.SimpleOrderOrchestrator.Sdek as Sdek
import Infrastructure.Utils.Http (HttpError)
import API.Types (OrderRequest (..), OrderConfirmationDetails (..))
import Infrastructure.Services.Yandex (getNearestSource)
import Infrastructure.Database (getOrderItems)
import Infrastructure.Services.Yandex.Types
import Infrastructure.Services.Yandex.Order
import Domain.Services.Warehouse (ensureWarehousePlatformId)
import Infrastructure.Services.Yandex.Types.Enums (Tariff (SelfPickup))
import Infrastructure.Services.Telegram (MessageIdResponse (..))
import Infrastructure.Services.Types (PaymentProvider (Tinkoff))
import Infrastructure.Database (Order, OrderItem (..), NewPaymentRecord (..), oiTotalPrice, finalizeYandexOrderRegistration, YandexOrder (..), Order (..))


data PlaceOrderError =
    --    YandexHttpError HttpError
    --  | DropOffPointNotFound
       WarehouseNotSet
     | DatabaseFailed Text
     | CartEmpty
     | TinkoffHttpError HttpError
     | TinkoffPaymentLinkFailed Text
     | NotificationSendFailed Text
     deriving Show

wrap action error = withExceptT error (ExceptT action)
{-# INLINE wrap #-}

-- Helper for kopecks
toKopecks :: Double -> Int32
toKopecks = round . (* 100)

place :: OrderRequest -> ExceptT PlaceOrderError AppM OrderConfirmationDetails
place orderRequest@OrderRequest {..} = do
--   maybeDropOffPoint <- wrap getNearestSource YandexHttpError
--   when(isNothing maybeDropOffPoint) $ throwE DropOffPointNotFound
--   let Just (sourceAddress, sourcePointId) = maybeDropOffPoint

--   $(logTM) InfoS $ "Yandex source point is at address: " <> ls (tshow sourceAddress)

  -- drop off point (platform station id)
  stateVar <- lift get
  maybeWarehouseId <- fmap _yandexWarehouseId $ lift $ readTVarIO stateVar
  let eWarehouseId =
        case maybeWarehouseId of
         Nothing            -> Left WarehouseNotSet
         Just warehouseId   -> Right warehouseId
  sourcePointId <- except eWarehouseId

  cfg <- lift ask
  let pool = _appDBPool cfg
    -- STEP A. Fetch items from the cart
  items <- wrap (getOrderItems orTelegramUserId pool) DatabaseFailed

  when (length items == 0) $ except $ Left CartEmpty
  
 -- STEP B. Generate the payment link
  let tinkoffCred = _tinkoffCred cfg
  orderId <- liftIO generateOrderId
  let initReq = Sdek.mkInitRequest orderId items orCustomerPhone tinkoffCred

  $(logTM) InfoS $ ls $ "initReq: " <> encodePretty initReq
  tinkoffResp :: Tinkoff.InitResponse <- wrap (Tinkoff.initiateTinkoffPayment initReq) TinkoffHttpError

  $(logTM) InfoS $ "Tinkoff response received. " <> ls (show tinkoffResp)

  when (Tinkoff.irSuccess tinkoffResp == False) $ do
    let errMsg = "Tinkoff Init API call failed: " <> fromMaybe "Unknown error" (Tinkoff.irMessage tinkoffResp)
    void $ wrap (pure (Left ())) (const $ TinkoffPaymentLinkFailed errMsg)

  paymentLink <- wrap ( 
    case Tinkoff.irPaymentURL tinkoffResp of
      Just link -> pure (Right link)
      Nothing   -> pure (Left ())
    ) (const $ TinkoffPaymentLinkFailed "Tinkoff Init API did not return a payment URL.")
   
  let tinkoffPaymentId = fromJust (Tinkoff.irPaymentId tinkoffResp)

  -- STEP C. Generate QR code
  let qrReq = 
        Tinkoff.defGetQrRequest 
        { Tinkoff.gqrTerminalKey = tinkoffTerminalKey tinkoffCred
        , Tinkoff.gqrPaymentId = read @Int64 (T.unpack tinkoffPaymentId)
        , Tinkoff.gqrToken =
           Tinkoff.generateGetQrToken $
              Tinkoff.GetQrToken
              tinkoffPaymentId
              (tinkoffTerminalKey tinkoffCred)
              (tinkoffSecret tinkoffCred)
              Tinkoff.PAYLOAD
        }

  $(logTM) InfoS $ ls $ "QR req: " <> encodePretty qrReq
  tinkoffQrResp :: Tinkoff.GetQrResponse <- wrap (Tinkoff.getTinkoffQRCode qrReq) TinkoffHttpError

  when(Tinkoff.gqrrSuccess tinkoffQrResp == False) $
    $(logTM) ErrorS $ "Tinkoff QR fails. " <> ls (show tinkoffQrResp)

  let linkToQr = Tinkoff.gqrrData tinkoffQrResp

  -- STEP D. Notify the telegram channel
  telegramMsgId <- fmap coerce $ wrap (Sdek.notifyOrdersChannel orderRequest items orderId) NotificationSendFailed

  let getStateRequest = 
        Tinkoff.GetStateRequest
        { gsrqPaymentId = tinkoffPaymentId
        , gsrqToken = 
            Tinkoff.generateGetStateToken $
              Tinkoff.GetStateToken
              tinkoffPaymentId
              (tinkoffTerminalKey tinkoffCred)
              (tinkoffSecret tinkoffCred)
        , gsrqTerminalKey = tinkoffTerminalKey tinkoffCred
        , gsrqIP = Nothing
        }
  st <- get  
  lift $ readTVarIO st >>= ((`writeTChanIO` (ShipNow, orderId, getStateRequest)) . _tinkoffPaymentChan)


  -- STEP E. Store draft of order request in db
  let orderDraftJson = 
        toJSON $
         YandexCreateOrderReq
         { info          = defRequestInfo { riOperatorRequestId = orderId }
         , source        = SourceRequestNode (PlatformStation (platformStationId sourcePointId))
         , destination   = defDestinationRequestNode { drnPlatformStation = Just (PlatformStation orDeliveryPointId) }
         , billingInfo   = defBillingInfo
         , items         = 
             items <&> \OrderItem {..} -> 
               Item 
               { iCount          = 1
               , iName           = oiName
               , iArticle        = oiArticle
               , iBillingDetails =
                  ItemBillingDetails 
                  { ibdUnitPrice         = 0
                  , ibdAssessedUnitPrice = toKopecks oiTotalPrice
                  }
               , iPlaceBarcode   = orderId
               }
         , places         = [defPlace]
         , recipientInfo  = RecipientInfo orCustomerFullName orCustomerPhone
         , lastMilePolicy = SelfPickup
         }

  let totalPrice = sum [ oiTotalPrice item | item <- items]
  let newPaymentRecord = 
        NewPaymentRecord
        { nprOrderId           = Just orderId
        , nprProvider          = Tinkoff
        , nprProviderPaymentId = tinkoffPaymentId
        , nprAmountKopecks     = round totalPrice
        , nprPaymentUrl        = paymentLink
        , nprError             = Nothing
        , nprToken             = Tinkoff.irToken initReq
        , nprPaymentFlow       = encodeToText ShipNow
        , nprShelfOrderId      = Nothing
        }

  let dbOrder = mkDbOrder orderRequest orderId telegramMsgId orderDraftJson
  void $ wrap (finalizeYandexOrderRegistration dbOrder newPaymentRecord pool) DatabaseFailed

  let trackingNumber = Nothing
  return OrderConfirmationDetails {..}


mkDbOrder :: OrderRequest -> Text -> Int64 -> Value -> Order
mkDbOrder OrderRequest {..} orderId msgId draftJson =
  let yandexOrder = 
        YandexOrder
        { yaDeliveryPoint  = orDeliveryPointId
        , yaTariff         = SelfPickup
        , yaDraftJson      = draftJson
        }
  in
    Order 
    { _orderId                            = orderId
    , _orderCustomerFullName              = orCustomerFullName
    , _orderCustomerPhone                 = orCustomerPhone
    , _orderDeliveryProviderId            = encodeToText orDeliveryProviderId
    , _orderInternalNotificationMessageId = msgId
    , _orderTelegramUserId                = orTelegramUserId
    , _orderSdek                          = Nothing
    , _orderYandex                        = Just yandexOrder
    }
