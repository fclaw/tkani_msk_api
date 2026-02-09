{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Domain.Services.Shipping (prepareAndSchedulePickup) where


import Data.Text (Text)
import Data.UUID (UUID)
import Data.Maybe (fromJust)
import Control.Monad (void)
import Data.Traversable (for)
import Data.Functor ((<&>))
import Data.Bifunctor (first, second)
import System.Timeout (timeout)
import Control.Monad.IO.Class (liftIO)
import Katip (logTM, Severity(..), ls)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Monad.Trans.Class (lift)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad.Trans.Except
import Data.Time.Calendar (addDays)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, takeTMVar)
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay, Day)


import App
import Text (tshow, encodeToText)
import Infrastructure.Services.Sdek.Types.State
import Infrastructure.Services.Sdek.Types
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, sendDocument)
import Infrastructure.Services.Sdek.Types.Courier
import Workers.SdekPriceCalculator (registerReceipt)
import Workers.SdekGenerateReceipt (getSdekReceipt, downloadSdekPdf)
import Infrastructure.Services.Sdek.Types.State (SdekRequestState)
import API.Handlers.PlaceNewOrder (PlaceOrderError (..), fetchOrderPollerRes)
import Infrastructure.Services.Sdek  (registerOrder, registerCourierCall)
import Infrastructure.Services.Sdek.Types.Config (SdekConfig (..), Sender (..), SdekSenderLocation (..))
import Infrastructure.Database (OrdersForCourierPickup (..), OrdersForCourierPickupItem (..), fetchOrdersForCourierPickup, createCourierPickupPromise)


data CourierCall = 
     CourierCall
     { orderUuid :: Maybe UUID
     , appUuid   :: Maybe UUID
     , state     :: SdekRequestState
     , status    :: SdekPickupAppStatus
     }


prepareAndSchedulePickup :: AppM Bool
prepareAndSchedulePickup = do
    $(logTM) InfoS "Checking for paid orders to schedule for pickup..."
    -- Get the current date to pass to the query for the idempotency check
    today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
    -- 1. Atomically find and update the orders.
    --    The query now has built-in guards.
    cfg <- ask
    let pool = _appDBPool cfg
    let sdekConfig = _sdekConfig cfg
    let minCourierPickup = pickupMinimum sdekConfig
    eOrdersToSchedule <- fetchOrdersForCourierPickup pool
    case eOrdersToSchedule of
      Left dbErr -> fmap (const False) $ $(logTM) ErrorS $ ls $ "DB error while fetching paid orders: " <> tshow dbErr
      Right orders ->
        if null orders then
          fmap (const False) $ $(logTM) InfoS $ "No new paid orders to schedule."
        else if length orders < minCourierPickup then
          fmap (const False) $ 
            $(logTM) InfoS $ ls $
              "Found only " <> tshow (length orders) <>
              " orders, which is below the minimum threshold of " <>
              tshow minCourierPickup <>
              " for courier pickup. Skipping scheduling."
        else do
          -- We have enough orders to schedule a pickup
          $(logTM) InfoS "Scheduling courier pickup for orders..."          
          -- ... (the rest of your logic to call the SDEK API) ...
          $(logTM) InfoS $ ls $ "Found " <> tshow (length orders) <> " orders. Scheduling courier..."
          let recipient = SdekRecipient (name (sender sdekConfig)) [SdekPhone (phone (sender sdekConfig))]
          let SdekSenderLocation {..} = senderLocation sdekConfig
          let location = SdekFromLocation address cityCode (Just postalCode)
          let pickupOderRequest = mkPickupOderRequest location (courierDropOffPoint sdekConfig) recipient orders
          $(logTM) InfoS $ ls $ "pretty print pickupOderRequest: " <> encodePretty pickupOderRequest
          eResp <- runExceptT $ tryRegisteringCourierCall pickupOderRequest
          case eResp of
            Left err -> do
              $(logTM) ErrorS $ ls $ "tryRegisteringCourierCall failed: " <> tshow err
              let error = escapeMarkdownV2 $ "‼️ Error in calling tryRegisteringCourierCall: " <> tshow err
              fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
            Right CourierCall {..} ->
              case state of
                Invalid -> do
                  $(logTM) ErrorS $ "tryRegisteringCourierCall failed: invalid state"
                  let error = escapeMarkdownV2 $ "‼️ Error in calling tryRegisteringCourierCall: invalid state"
                  fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
                Successful -> do
                  let app_uuid = fromJust appUuid
                  let order_uuid = fromJust orderUuid
                  let statusTxt = tshow status
                  let orderIds = orders <&> \OrdersForCourierPickup {..} -> ocpOrderId
                  eDbRes <- createCourierPickupPromise order_uuid app_uuid statusTxt orderIds (addDays 1 today) pool
                  case eDbRes of
                    Left dbErr -> do
                      $(logTM) ErrorS $ ls $ "DB error while creating courier pickup promise: " <> tshow dbErr
                      let error = escapeMarkdownV2 $ "‼️ Error in calling createCourierPickupPromise: " <> tshow dbErr
                      fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
                    Right _ -> do
                      eReceiptRes <- registerReceipt order_uuid
                      case eReceiptRes of
                        Left err -> do
                          $(logTM) ErrorS $ ls $ "registerReceipt failed: " <> tshow err
                          let error = escapeMarkdownV2 $ "‼️ Error in calling registerReceipt: " <> tshow err
                          fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
                        Right receipt_uuid -> do
                          eUrlRes <-getSdekReceipt receipt_uuid
                          case eUrlRes of
                            Left err -> do
                              $(logTM) ErrorS $ ls $ "getSdekReceipt failed: " <> tshow err
                              let error = escapeMarkdownV2 $ "‼️ Error in calling getSdekReceipt: " <> tshow err
                              fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
                            Right url -> do
                              ePdfRes <- downloadSdekPdf url
                              case ePdfRes of
                                Left err -> do
                                  $(logTM) ErrorS $ ls $ "downloadSdekPdf failed: " <> tshow err
                                  let error = escapeMarkdownV2 $ "‼️ Error in calling downloadSdekPdf: " <> tshow err
                                  fmap (const False) $ sendOrEditTelegramMessage mempty error PICKUP Nothing Nothing Nothing
                                Right pdfBytes -> do
                                  let caption = "the courier call has been registered for " <> escapeMarkdownV2 (tshow (addDays 1 today))
                                  let filename = "pickup-manifest-" <> tshow today <> ".pdf"
                                  -- 2. Call the new service function
                                  void $ sendDocument PICKUP caption filename pdfBytes "application/pdf"
                                  fmap (const True) $ $(logTM) InfoS $ "Successfully sent  pickup manifest for " <> ls (tshow today) <> " to pickup channel."


mkPickupOderRequest :: SdekFromLocation -> Text -> SdekRecipient -> [OrdersForCourierPickup] -> SdekOrderRequest
mkPickupOderRequest location dropOffPoint recipient orders =
  let packages = 
        orders <&> \OrdersForCourierPickup {..} ->
        let pkgNumber = ocpOrderId
            pkgWeight = fromIntegral ocpWeight
            pkgItems  = 
              ocpItems <&> \OrdersForCourierPickupItem {..} ->
                let pkiName    = ocpiName
                    pkiWareKey = ocpiArticle
                    pkiPayment = 
                      SdekPayment
                      { payValue = 0
                      , vatSum = Nothing
                      , vatRate = Nothing
                      }
                    pkiWeight = fromIntegral ocpWeight
                    pkiAmount = 1
                    pkiCost   = 0                      
                in SdekPackageItem {..}
            pkgLength = Just $ fromIntegral ocpLength
            pkgWidth  = Just $ fromIntegral ocpWidth
            pkgHeight = Just $ fromIntegral ocpHeight
        in SdekPackage {..}
  in
    SdekOrderRequest
    { sorTariffCode    = 138 -- Courier pickup tariff code
    , sorRecipient     = recipient
    , sorPackages      = packages
    , sorShipmentPoint = Nothing
    , sorFromLocation  = Just location
    , sorDeliveryPoint = dropOffPoint
    , sorServices      = []
    }

wrap action error = withExceptT error (ExceptT action)

fetchCourierPollerRes :: UUID -> ExceptT PlaceOrderError AppM (Either Text (SdekRequestState, SdekPickupAppStatus))
fetchCourierPollerRes uuid = do
  st <- get
  inChan <- fmap _sdekCourierChan $ lift $ readTVarIO st -- The poller's INput chan
  -- 1. Create a new, empty TMVar for the reply
  replyVar <- liftIO newEmptyTMVarIO

  -- 2. Create the job and put it on the poller's queue
  let job = SdekCourierJob uuid replyVar
  lift $ writeTChanIO inChan job

  -- 3. Block and wait for the result to appear in our reply box
  -- We use a timeout to prevent waiting forever.
  mResult <- liftIO $ timeout (30 * 1000000) $ atomically $ takeTMVar replyVar

  -- 4. Handle the outcome
  case mResult of
    -- Timeout occurred
    Nothing -> throwE SdekConfirmationTimeout
        
    -- We got a result from the poller
    Just result -> return result


-- uuid is required for a receipt
tryRegisteringCourierCall :: SdekOrderRequest -> ExceptT PlaceOrderError AppM CourierCall
tryRegisteringCourierCall orderReq = do
  order_uuid <- wrap (registerOrder orderReq) SdekRegistrationFailed
  ePollerRes <- fetchOrderPollerRes order_uuid
  -- for now we discard tracking number
  _ <- except $ (first SdekPollerError) ePollerRes
  SdekCourierResponse {..} <- wrap (registerCourierCall order_uuid) NetworkError
  let app_uuid = uuid entity
  eCourierPollerRes <- fetchCourierPollerRes app_uuid
  (state, status) <- except $ (first SdekPollerError) eCourierPollerRes
  return $  
    case state of
      Successful -> 
        CourierCall 
        { orderUuid = Just order_uuid
        , appUuid = Just app_uuid
        , state = state
        , status = status 
        }
      Invalid   -> CourierCall { orderUuid = Nothing, appUuid = Nothing, state = state, status = status }
  