{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections #-}


module Infrastructure.Services.Sdek 
       ( getDeliveryPoints
       , registerOrder
       , makeMinimalOrderRequestData
       , buildMinimalOderRequest
       , makeMinimalYamlOrderRequestData
       , makeMinimalShelfRequestData
       , getOrderStatus
       , getOrdersInTransit
       , getDeliveryPointByCode
       , getCityByName
       , getTotalSumByTariff
       , patchOrder
       , obtainOrderReceiptUrl
       , requestReceiptGeneration
       , cancelOrder
       , getAvailableTariffs
       , findOptimalTariff
       , registerCourierCall
       , getPickupApplication
       , getPickupApplicationStatus
       , cancelPickupApplication
       ) where

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Katip
import qualified Data.Text as T
import Control.Monad (forM_, void, when)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Concurrent.STM (atomically, readTVar)
import Data.Time (UTCTime, diffUTCTime)
import qualified Data.HashMap.Strict as HM
import Data.UUID (UUID)
import Data.Bifunctor (second)
import qualified Data.UUID as UUID
import Data.Traversable (for)
import Data.Time.Calendar (addDays)
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay)


import App (AppM, sdekAccessToken, _sdekConfig, _pointCache, currentTime, _configHttpManager, Scheme (HTTPS))
import API.Types
import Text (tshow)
import Data.Maybe (fromMaybe)
import TH.Location (currentModule)
import Infrastructure.Utils.Http
import API.WithField (WithField (..))
import Infrastructure.Services.Sdek.Types.Courier
import Infrastructure.Services.Sdek.Auth (getValidSdekToken)
import Infrastructure.Services.Sdek.CachedDeliveryPoints (storeDeliveryPoints)
import Infrastructure.Services.Sdek.Types hiding (DeliveryPoint)
import qualified Infrastructure.Services.Sdek.Types as Sdek (DeliveryPoint)
import Infrastructure.Services.Sdek.Types.OrderInTransit (SdekOrderInTransitResponse)
import Infrastructure.Database.Types (OrderItem (..))
import qualified Infrastructure.Services.Sdek.Types.Config as Sdek
import Infrastructure.Services.Sdek.Types.State (SdekRequestState (..))



findOptimalTariff :: [Sdek.Tariff] -> [Int] -> Int
findOptimalTariff [] _ = error "findOptimalTariff: not suitable tariff"
findOptimalTariff (tariff:rest) availableTariffs 
  | Sdek.tariffToInt tariff `elem` availableTariffs = Sdek.tariffToInt tariff
  | otherwise = findOptimalTariff rest availableTariffs


withToken mkRequest = 
  getValidSdekToken >>= \token ->
    let tkn = (Just . mkDefToken . sdekAccessToken) token
    in mkRequest tkn

getDeliveryPoints :: Text -> AppM (ApiResponse [WithField "dpMetros" [T.Text] DeliveryPoint])
getDeliveryPoints city = do
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  -- Step 1: Find the SDEK city code.
  $(logTM) InfoS $ logStr $ "Fetching SDEK city code for " <> city
  let cityUrl = show HTTPS <> url <> "/v2/location/cities"
  let cityParams =
        [ ("country_codes", "RU")  -- THE FIX: Limit search to Russia
        , ("city", city)           -- The city name to search for
        , ("size", "1")            -- Optional but good practice: we only need one result
        , ("lang", "rus")          -- Optional but good practice: ensure Russian response
        ]
  let httpManager = _configHttpManager cfg      
  let cityReq = withToken (_getReq' httpManager cityUrl cityParams [])
  eCities <- makeRequestWithRetries @[SdekCity] (Just (void $ getValidSdekToken)) cityReq
  handleApiResponse @_ @[SdekCity] $(currentModule) eCities $ \case
    [] -> do
      $(logTM) InfoS $ logStr $ "SDEK city not found for: " <> city
      pure $ Right [] -- Return an empty list, which is a valid success case.
    (_:_:_) -> do 
      $(logTM) InfoS $ logStr $ "SDEK city not found (no exact match): " <> city
      pure $ Right []
    (firstCity:_) -> do
        stateTVar <- get
        now <- currentTime
        -- Read the current cache content atomically
        currentCache <- fmap _pointCache $ liftIO $ atomically $ readTVar stateTVar
        -- Check if a valid, fresh entry exists
        case HM.lookup (code firstCity) currentCache of
          Just (timestamp, cachedPoints) | isFresh now timestamp -> do
            -- CACHE HIT
            $(logTM) InfoS $ "Cache hit for city: " <> ls city
            return $ Right cachedPoints
          _ -> do
            $(logTM) InfoS $ "Cache miss for city: " <> ls city <> ". Fetching from APIs..."
            storeDeliveryPoints $ code firstCity


-- A helper to define what "fresh" means (e.g., 6 hours)
isFresh :: UTCTime -> UTCTime -> Bool
isFresh now prev = let sixHours = 6 * 60 * 60 in diffUTCTime now prev < sixHours


-- | A clean, minimal record holding all the necessary data gathered from the bot
--   to construct an 'SdekOrderRequest'
data MinimalOrderRequestData = MinimalOrderRequestData
  { -- | The customer's full name as a single string.
    --   Source: User input from the bot's 'GET_FULL_NAME' state.
    mordName        :: Text

    -- | The customer's phone number, preferably normalized to a standard format.
    --   Source: User input from the bot's 'GET_PHONE' state.
  , mordPhone       :: Text

    -- | The unique code for the chosen SDEK delivery point (e.g., "MSK622").
    --   Note: This should be the code *without* any "sdek_" prefix.
    --   Source: User selection from the paginated list of delivery points.
  , mordDeliveryPointCode :: Text

  , mordTariffCode    :: Int
  , mordShipmentPoint :: Maybe Text
  , mordItems         :: [OrderItem]
  }


stripPrefix :: Text -> Text -> Text
stripPrefix prefix txt =
  -- T.stripPrefix is from Data.Text. It returns a 'Maybe Text'.
  -- If the prefix matches, it returns 'Just the_rest_of_the_string'.
  -- If it doesn't match, it returns 'Nothing'.
  case T.stripPrefix prefix txt of
    Just rest -> rest    -- Prefix matched, return the rest of the string.
    Nothing   -> txt     -- Prefix did not match, return the original string.


makeMinimalOrderRequestData :: OrderRequest -> [OrderItem] -> Int -> Maybe Text -> MinimalOrderRequestData
makeMinimalOrderRequestData OrderRequest {..} items tariffCode shipmentPoint =
  MinimalOrderRequestData 
  { mordName = orCustomerFullName
  , mordPhone = orCustomerPhone
  , mordDeliveryPointCode = 
      fromMaybe 
        orDeliveryPointId 
        (T.stripPrefix "sdek_" orDeliveryPointId)
  , mordTariffCode = tariffCode
  , mordShipmentPoint = shipmentPoint
  , mordItems = items
  }

makeMinimalShelfRequestData :: Text -> Text -> Text -> Int -> [OrderItem] -> Maybe Text -> MinimalOrderRequestData
makeMinimalShelfRequestData fullName phone deliverPoint tariffCode items shipmentPoint =
  MinimalOrderRequestData 
  { mordName = fullName
  , mordPhone = phone
  , mordDeliveryPointCode = deliverPoint
  , mordTariffCode = tariffCode
  , mordShipmentPoint = shipmentPoint
  , mordItems = items
  }


makeMinimalYamlOrderRequestData :: YamlOrderRequest -> Int -> Maybe Text -> MinimalOrderRequestData
makeMinimalYamlOrderRequestData YamlOrderRequest {..} tariffCode shipmentPoint =
  let indexedItems = zip [1 ..] yorItems
      items = 
         flip map indexedItems $ \(idx, YamlOrderItem {..}) ->
            OrderItem
            { oiName = yoiName
            , oiArticle = "ART-" <> tshow idx
            , oiFabricType = yoiFabricType
            , oiPricePerMetre = yoiPricePerMetre
            , oiTotalPrice = yoiTotalPrice
            , oiLengthM = yoiLengthM
            , oiTelegramUrl = mempty
            , oiThumbnailUrl = Nothing
            }
  in
    MinimalOrderRequestData 
    { mordName = yorCustomerFullName
    , mordPhone = yorCustomerPhone
    , mordDeliveryPointCode = 
        fromMaybe 
          yorDeliveryPointId 
          (T.stripPrefix "sdek_" yorDeliveryPointId)
    , mordTariffCode = tariffCode
    , mordShipmentPoint = shipmentPoint
    , mordItems = items
    }

-- | Builds the minimal SdekOrderRequest payload needed to register an order.
--   Offloads address/item details to be filled in manually later.
buildMinimalOderRequest :: MinimalOrderRequestData -> SdekOrderRequest
buildMinimalOderRequest MinimalOrderRequestData {..} =
  let
    -- 1. Create the recipient payload
    recipient = SdekRecipient
      { rcpName = mordName
      , rcpPhones = [SdekPhone mordPhone]
      }

    -- 2. Item info. Create a single, generic item for the fabric.
    items = flip map mordItems $ \OrderItem {..} ->
      SdekPackageItem
      { pkiName = oiName -- A generic name is fine for your manual workflow
      , pkiWareKey = oiArticle -- Use your internal fabric ID
      , pkiPayment = 
          SdekPayment 
          { payValue = 0
          , vatSum = Nothing
          , vatRate = Nothing 
          }
      , pkiWeight = 500 -- A sensible default weight in grams
      , pkiAmount = 1   -- It's one "item" (one piece of fabric)
      , pkiCost = round oiTotalPrice
      }

    totalPrice = sum [oiTotalPrice item | item <- mordItems]  

    -- 3. Create a default package payload
    --    You MUST provide an estimated weight. You can't skip this.
    --    A sensible default (e.g., 500g) is a good start.
    package = SdekPackage
      { pkgNumber = "1" -- Simple default for one-package orders
      , pkgWeight = 1 -- Default weight in grams
      , pkgItems = items
      , pkgLength = Nothing
      , pkgWidth = Nothing
      , pkgHeight = Nothing
      }
  in
    -- 3. Assemble the final request
    SdekOrderRequest
      { -- Hardcode the tariff for now if you only offer one type of delivery
        sorTariffCode = mordTariffCode -- e.g., "Посылка склад-ПВЗ"
      , sorRecipient = recipient
      , sorPackages = [package]
      , sorShipmentPoint = mordShipmentPoint
      , sorFromLocation = Nothing
      , sorDeliveryPoint = mordDeliveryPointCode
      , sorServices = [SdekService INSURANCE (Just (T.pack (show (totalPrice + 1))))]
      }


registerOrder :: SdekOrderRequest -> AppM (Either SdekError UUID)
registerOrder order = do
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  -- Step 1: Find the SDEK city code.
  $(logTM) InfoS $ logStr $ "registering order in sdek" <> show order
  let ordersUrl = show HTTPS <> url <> "/v2/orders"
  let httpManager = _configHttpManager cfg
  let ordersReq = withToken (_postReq' httpManager ordersUrl order [])
  eOrders <- makeRequestWithRetries @SdekOrderResponse (Just (void $ getValidSdekToken)) ordersReq
  handleApiResponse @_ @SdekOrderResponse $(currentModule) eOrders $ \resp -> do
    when(sorRequestState resp == Accepted) $ 
      $(logTM) InfoS $ logStr $ "sdek has responded positively with uuid: " <> show (sorEntityUuid resp)
    return $
      case sorRequestState resp of
        Accepted -> do
          -- This is the happy path. We return the UUID to be used by the poller.
          Right $ sorEntityUuid resp
        Invalid ->
          -- The request was invalid. Now we check for the error details.
          Left $ case sorErrors resp of
            -- THIS IS YOUR NEW, ROBUST LOGIC:
            -- If the errors array is empty, we create our own, informative error.
            Just [] -> SdekError
                    { seCode = "INVALID_STATE_NO_DETAILS"
                    , seMessage = "SDEK reported an INVALID status but provided no error details."
                    }
            -- If there are errors, we just take the first one.
            Just (firstError:_) -> firstError
            Nothing -> SdekError 
                    { seCode = "INVALID ERROR STATE"
                    , seMessage = "status Invalid, but errors list is empty" 
                    }
        -- Handle other unexpected states gracefully
        otherState ->
          Left $ SdekError
            { seCode = "UNEXPECTED_STATE"
            , seMessage = "SDEK returned an unexpected initial state: " <> T.pack (show otherState)
            }


getOrderStatus :: UUID -> AppM (Either SdekError SdekOrderStatusResponse)
getOrderStatus uuid = do
  $(logTM) DebugS $ "Polling SDEK for status of order UUID: " <> ls (UUID.toText uuid)
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let fullUrl = show HTTPS <> url <> "/v2/orders/" <> UUID.toString uuid
  let httpManager = _configHttpManager cfg
  let ordersReq = withToken (_getReq' httpManager fullUrl mempty [])
  eOrders <- makeRequestWithRetries @SdekOrderStatusResponse (Just (void $ getValidSdekToken)) ordersReq
  handleApiResponse @_ @SdekOrderStatusResponse $(currentModule) eOrders $ pure . Right


getOrdersInTransit :: UUID -> AppM (Either HttpError SdekOrderInTransitResponse)
getOrdersInTransit uuid = do
  $(logTM) DebugS $ "Polling SDEK for status of order UUID: " <> ls (UUID.toText uuid)
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let fullUrl = show HTTPS <> url <> "/v2/orders/" <> UUID.toString uuid
  let httpManager = _configHttpManager cfg
  let ordersReq = withToken (_getReq' httpManager fullUrl mempty [])
  makeRequestWithRetries @SdekOrderInTransitResponse (Just (void $ getValidSdekToken)) ordersReq


getDeliveryPointByCode :: Text -> AppM (Either HttpError [Sdek.DeliveryPoint])
getDeliveryPointByCode code = do
  cfg <- ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let httpManager = _configHttpManager cfg
  let pointsUrl = show HTTPS <> url <> "/v2/deliverypoints"
      -- --- THIS IS THE FIX ---
    -- We remove the 'type' and 'size' parameters.
    -- When querying by a unique code, we don't need to filter by type.
    -- 'size' is also irrelevant as we expect only one result.
    -- When searching by a unique 'code', we should NOT filter by 'type'.
    -- This allows the API to return the result whether it's a PVZ or a POSTAMAT.
  let params = [("code", code)]
  let pointsReq = withToken (_getReq' httpManager pointsUrl params [])
  ePoints <- makeRequestWithRetries @[Sdek.DeliveryPoint] (Just (void $ getValidSdekToken)) pointsReq
  handleApiResponse @_ @[Sdek.DeliveryPoint] $(currentModule) ePoints $ pure . Right

getCityByName :: Text -> AppM (Either HttpError [SdekCityWithCode])
getCityByName cityName = do
  cfg <- ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let httpManager = _configHttpManager cfg
  let cityUrl = show HTTPS <> url <> "/v2/location/cities"
  let params = -- The query parameters for the API call
        [ ("country_codes", ("RU" :: Text)) -- Best practice to limit search to Russia
        , ("city", cityName)
        , ("size", tshow 1)
        ]
  let pointsReq = withToken (_getReq' httpManager cityUrl params [])
  ePoints <- makeRequestWithRetries @[SdekCityWithCode] (Just (void $ getValidSdekToken)) pointsReq
  handleApiResponse @_ @[SdekCityWithCode] $(currentModule) ePoints $ pure . Right


getTotalSumByTariff :: TotalSumRequest -> AppM (Either HttpError TotalSumResponse)
getTotalSumByTariff totalSum = do
  cfg <- ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let httpManager = _configHttpManager cfg
  let tariffUrl = show HTTPS <> url <> "/v2/calculator/tariff"
  let totalSumReq = withToken (_postReq' httpManager tariffUrl totalSum [])
  makeRequestWithRetries @TotalSumResponse (Just (void $ getValidSdekToken)) totalSumReq


patchOrder :: PatchedOrderRequest -> AppM (Either HttpError PatchedOrderResponse)
patchOrder patchedOrder = do
  cfg <- ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let httpManager = _configHttpManager cfg
  let patchUrl = show HTTPS <> url <> "/v2/orders"
  let patchReq = withToken (_patchReq' httpManager patchUrl patchedOrder [])
  makeRequestWithRetries @PatchedOrderResponse (Just (void $ getValidSdekToken)) patchReq

obtainOrderReceiptUrl :: UUID -> AppM (Either HttpError ReceiptStatusResponse)
obtainOrderReceiptUrl uuid = do
  cfg <- ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let httpManager = _configHttpManager cfg
  let receiptUrl = show HTTPS <> url <> "/v2/print/orders/" <> show uuid
  let receiptReq = withToken (_getReq' httpManager receiptUrl [] [])
  makeRequestWithRetries @ReceiptStatusResponse (Just (void $ getValidSdekToken)) receiptReq


requestReceiptGeneration :: UUID -> AppM (Either HttpError ReceiptRegisterResponse)
requestReceiptGeneration uuid = do
  cfg <- ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let httpManager = _configHttpManager cfg
  let printfUrl = show HTTPS <> url <> "/v2/print/orders"
  let orders = ReceiptRegisterRequest [ReceiptRegisterRequestOrder uuid] 2
  let totalSumReq = withToken (_postReq' httpManager printfUrl orders [])
  makeRequestWithRetries @ReceiptRegisterResponse (Just (void $ getValidSdekToken)) totalSumReq


cancelOrder :: UUID -> AppM (Either HttpError CancelOrderResponse)
cancelOrder uuid = do
  cfg <- ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let httpManager = _configHttpManager cfg
  let cancelUrl = show HTTPS <> url <> "/v2/orders/" <> show uuid
  let cancelReq = withToken (_deleteReq' httpManager cancelUrl [])
  makeRequestWithRetries @CancelOrderResponse (Just (void $ getValidSdekToken)) cancelReq


getAvailableTariffs :: Location -> Location -> AppM (Either HttpError AvailableTariffsResponse)
getAvailableTariffs fromLocation toLocation = do
  cfg <- ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let httpManager = _configHttpManager cfg
  let maxWeight = 20000
  let printfUrl = show HTTPS <> url <> "/v2/calculator/tarifflist"
  let tariff = AvailableTariffsRequest  fromLocation toLocation [Package maxWeight 0 0 0]
  let totalSumReq = withToken (_postReq' httpManager printfUrl tariff [])
  makeRequestWithRetries @AvailableTariffsResponse (Just (void $ getValidSdekToken)) totalSumReq


registerCourierCall :: UUID -> AppM (Either HttpError SdekCourierResponse)
registerCourierCall uuid = do
  today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  let tomorrow = addDays 1 today
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let courierUrl = show HTTPS <> url <> "/v2/intakes"
  let httpManager = _configHttpManager cfg
  let sdekConfig = _sdekConfig cfg
  let sdekCourierRequest = 
        SdekCallCourierRequest
        { sccrOrderUuid = uuid
        , sccrIntakeDate = tshow tomorrow
        , sccrIntakeTimeFrom = Sdek.from (Sdek.pickupWindow sdekConfig)
        , sccrIntakeTimeTo = Sdek.to (Sdek.pickupWindow sdekConfig)
        , sccrSender = 
           SdekCallCourierSender
           { scsName = Sdek.name (Sdek.sender sdekConfig)
           , scsPhones = [SenderPhone (Sdek.phone (Sdek.sender sdekConfig))]
           }
        }
  let courierReq = withToken (_postReq' httpManager courierUrl sdekCourierRequest [])
  makeRequestWithRetries @SdekCourierResponse (Just (void $ getValidSdekToken)) courierReq

getPickupApplication :: UUID -> AppM (Either HttpError SdekPickupApplicationResponse)
getPickupApplication uuid = do
  $(logTM) DebugS $ "SDEK: pickup application UUID: " <> ls (UUID.toText uuid)
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let fullUrl = show HTTPS <> url <> "/v2/intakes/" <> UUID.toString uuid
  let httpManager = _configHttpManager cfg
  let pickupReq = withToken (_getReq' httpManager fullUrl mempty [])
  makeRequestWithRetries @SdekPickupApplicationResponse (Just (void $ getValidSdekToken)) pickupReq


getPickupApplicationStatus :: UUID -> AppM (Either HttpError SdekPickupAppStatusResponse)
getPickupApplicationStatus uuid = do
  $(logTM) DebugS $ "SDEK: status for pickup application UUID: " <> ls (UUID.toText uuid)
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let fullUrl = show HTTPS <> url <> "/v2/intakes/" <> UUID.toString uuid
  let httpManager = _configHttpManager cfg
  let pickupReq = withToken (_getReq' httpManager fullUrl mempty [])
  makeRequestWithRetries @SdekPickupAppStatusResponse (Just (void $ getValidSdekToken)) pickupReq

cancelPickupApplication :: UUID -> AppM (Either HttpError ())
cancelPickupApplication uuid = do
  $(logTM) DebugS $ "SDEK: cancelling pickup application UUID: " <> ls (UUID.toText uuid)
  cfg <- ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let fullUrl = show HTTPS <> url <> "/v2/intakes/" <> UUID.toString uuid
  let httpManager = _configHttpManager cfg
  let pickupCancelReq = withToken (_deleteReq' httpManager fullUrl mempty)
  fmap (second (const ())) $ makeRequestWithRetries @SdekCourierResponse (Just (void $ getValidSdekToken)) pickupCancelReq
