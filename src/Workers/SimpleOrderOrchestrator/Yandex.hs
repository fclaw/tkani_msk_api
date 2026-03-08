{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Workers.SimpleOrderOrchestrator.Yandex (place) where


import Katip (logTM, Severity (..), ls)
import Data.Text (Text)
import Data.Int (Int64)
import qualified Data.Text as T
import Control.Monad (void, when)
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Control.Monad.Trans.Except
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Class (ask)

import App
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


data PlaceOrderError =
       NetworkError HttpError
     | DropOffPointNotFound
     | DatabaseFailed Text
     | CartEmpty
     | TinkoffHttpError HttpError
     | TinkoffPaymentLinkFailed Text
     | NotificationSendFailed Text
     deriving Show

wrap action error = withExceptT error (ExceptT action)
{-# INLINE wrap #-}

place ::  OrderRequest -> ExceptT PlaceOrderError AppM OrderConfirmationDetails
place orderRequest@OrderRequest {..} = do
  maybeDropOffPoint <- wrap getNearestSource NetworkError
  when(isNothing maybeDropOffPoint) $ throwE DropOffPointNotFound
  let Just (_, pointId) = maybeDropOffPoint

  cfg <- lift ask
  let pool = _appDBPool cfg
    -- fetch total price for a given fabric
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
  telegramMsgId <- wrap (Sdek.notifyOrdersChannel orderRequest items orderId) NotificationSendFailed

  let trackingNumber = Nothing
--   let draft = YandexCreateOrderReq
  return OrderConfirmationDetails {..}
