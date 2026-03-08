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
import Workers.PriceCalculator (registerSdekReceipt)
import Workers.SdekGenerateReceipt (getSdekReceipt, downloadSdekPdf)
import Infrastructure.Services.Sdek.Types.State (SdekRequestState)
import Workers.SimpleOrderOrchestrator.Sdek (PlaceOrderError (..), fetchOrderPollerRes)
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
    let countThreshold = pickupParcels sdekConfig
    let weightThreshold = pickupWeight sdekConfig
    eOrdersToSchedule <- fetchOrdersForCourierPickup pool
    case eOrdersToSchedule of
      Left dbErr -> fmap (const False) $ $(logTM) ErrorS $ ls $ "DB error while fetching paid orders: " <> tshow dbErr
      Right orders ->
        if null orders then
          fmap (const False) $ $(logTM) InfoS $ "No new paid orders to schedule."
        else if not (checkRequirements orders countThreshold weightThreshold) then do
          -- --- This is the refined logging message ---
          let totalWeight =  sum $ orders <&> \OrdersForCourierPickup {..} -> ocpWeight
          let totalParcelsCount = length orders
          let notMetMsg = 
                "Requirements not met to call courier. " <>
                "Current state: " <>
                "Parcels Count = " <> 
                tshow totalParcelsCount <> 
                " (Threshold = " <> 
                tshow countThreshold <> "), " <>
                "Total Weight = " <>
                tshow totalWeight <> 
                " g (Threshold = " <> 
                tshow weightThreshold <> " g). " <>
                "Waiting for more orders or a heavier batch."
          $(logTM) InfoS $ ls notMetMsg
          fmap (const False) $ sendOrEditTelegramMessage mempty notMetMsg PICKUP Nothing Nothing Nothing
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
                      eReceiptRes <- registerSdekReceipt order_uuid
                      case eReceiptRes of
                        Left err -> do
                          $(logTM) ErrorS $ ls $ "registerSdekReceipt failed: " <> tshow err
                          let error = escapeMarkdownV2 $ "‼️ Error in calling registerSdekReceipt: " <> tshow err
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


-- | Checks if the given list of orders meets the requirements to call a courier.
--
-- Conditions:
--   1. Total parcels count exceeds 'countThreshold'.
--   2. OR total parcels count is less than or equal to 'countThreshold'
--      AND the total weight exceeds 'weightThreshold'.
--
-- This implies that if you have many parcels, call a courier regardless of weight.
-- But if you have few parcels, only call a courier if they are collectively heavy enough.
--
checkRequirements ::  [OrdersForCourierPickup] -> Int -> Int -> Bool
checkRequirements orders countThreshold weightThreshold =
  let totalWeight = fromIntegral $ sum $ orders <&> \OrdersForCourierPickup {..} -> ocpWeight
      totalParcelsCount = length orders
  in
    -- --- Implementation of your conditions ---
    -- Condition 1: parcels > countThreshold -> true
    (totalParcelsCount > countThreshold)
    -- OR
    ||
    -- Condition 2: parcels <= countThreshold && totalWeight > weightThreshold -> true
    (totalParcelsCount <= countThreshold && totalWeight > weightThreshold)


mkPickupOderRequest :: SdekFromLocation -> Text -> SdekRecipient -> [OrdersForCourierPickup] -> SdekOrderRequest
mkPickupOderRequest location dropOffPoint recipient orders =
  let packages = 
        orders <&> \OrdersForCourierPickup {..} ->
        let pkgNumber = ocpOrderId
            pkgWeight = fromIntegral ocpWeight + 100 -- safety margin
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
                    pkiWeight = fromIntegral ocpiWeight
                    pkiAmount = 1
                    pkiCost   = 0                      
                in SdekPackageItem {..}
            pkgLength = Just $ fromIntegral ocpLength
            pkgWidth  = Just $ fromIntegral ocpWidth
            pkgHeight = Just $ fromIntegral ocpHeight
        in SdekPackage {..}
  in
    -- Constructs a SdekOrderRequest for a "door-to-warehouse" (pickup to SDEK point) shipment.
    --  This request defines the pickup details, the specific tariff, and the items being sent.
    SdekOrderRequest
    { sorTariffCode    = 158
      -- ^ **CRITICAL NOTE:** Tariff code 158 corresponds to "Забор груза дверь-склад" (Courier pickup door-warehouse).
      --   This code was determined via network inspection as it is not consistently
      --   documented or appears as code 138 in some SDEK API versions/docs.
      --   It is used to explicitly request a SDEK courier pickup service from our location.
    
    , sorRecipient     = recipient -- Details of the final recipient of the parcel.
    , sorPackages      = packages  -- List of items to be picked up, including weight/dimensions.
    
    , sorShipmentPoint = Nothing   -- For 'дверь-склад', this means the pickup location is provided in 'fromLocation'.
                                   -- This would be 'Just pointId' if shipping FROM a SDEK PVZ.
    
    , sorFromLocation  = Just location -- Our warehouse location for the pickup.
    , sorDeliveryPoint = dropOffPoint  -- The SDEK pickup point (PVZ) where the customer will collect.
    
    , sorServices      = []        -- Additional SDEK services (e.g., insurance, fragile handling).
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
  