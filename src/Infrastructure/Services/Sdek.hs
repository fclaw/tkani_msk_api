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
       , getOrderStatus
       , getOrdersInTransit
       , scheduleSingleOrderCourier
       , getPickupApplicationByUUID
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
import qualified Data.UUID as UUID
import Data.Traversable (for)
import Data.Time (getZonedTime, zonedTimeToLocalTime, localDay)
import Data.Bifunctor (second)


import App (AppM, sdekAccessToken, _sdekConfig, _pointCache, currentTime, _configHttpManager, Scheme (HTTPS))
import API.Types
import Text (tshow)
import Infrastructure.Utils.Http
import Infrastructure.Services.Sdek.Auth (getValidSdekToken)
import TH.Location (currentModule)
import API.WithField (WithField (..))
import Infrastructure.Services.Sdek.CachedDeliveryPoints (storeDeliveryPoints)
import Infrastructure.Services.Sdek.Types
import Data.Maybe (fromMaybe)
import Infrastructure.Services.Sdek.Types.OrderInTransit (SdekOrderInTransitResponse)
import Infrastructure.Database.Types (OrderItem (..))
import qualified Infrastructure.Services.Sdek.Types.Config as Sdek
import Infrastructure.Services.Sdek.Types.Courier
import Infrastructure.Services.Sdek.Types.State (SdekRequestState (..))


getDeliveryPoints :: Text -> AppM (ApiResponse [WithField "dpMetros" [T.Text] DeliveryPoint])
getDeliveryPoints city = do
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  -- Step 1: Find the SDEK city code.
  $(logTM) InfoS $ logStr $ "Fetching SDEK city code for " <> city
  let cityUrl = "https://" <> url <> "/v2/location/cities"
  let cityParams =
        [ ("country_codes", "RU")  -- THE FIX: Limit search to Russia
        , ("city", city)           -- The city name to search for
        , ("size", "1")            -- Optional but good practice: we only need one result
        , ("lang", "rus")          -- Optional but good practice: ensure Russian response
        ]
  let httpManager = _configHttpManager cfg      
  let cityReq = getValidSdekToken >>= (_getReq' httpManager cityUrl cityParams . Just . sdekAccessToken)
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

  , mordTariffCode :: Int
  , mordFromLocation :: SdekFromLocation
  , mordItems :: [OrderItem]
  }


stripPrefix :: Text -> Text -> Text
stripPrefix prefix txt =
  -- T.stripPrefix is from Data.Text. It returns a 'Maybe Text'.
  -- If the prefix matches, it returns 'Just the_rest_of_the_string'.
  -- If it doesn't match, it returns 'Nothing'.
  case T.stripPrefix prefix txt of
    Just rest -> rest    -- Prefix matched, return the rest of the string.
    Nothing   -> txt     -- Prefix did not match, return the original string.


makeMinimalOrderRequestData :: OrderRequest -> [OrderItem] -> Int -> SdekFromLocation -> MinimalOrderRequestData
makeMinimalOrderRequestData OrderRequest {..} items tariffCode fromLocation =
  MinimalOrderRequestData 
  { mordName = orCustomerFullName
  , mordPhone = orCustomerPhone
  , mordDeliveryPointCode = 
      fromMaybe 
        orDeliveryPointId 
        (T.stripPrefix "sdek_" orDeliveryPointId)
  , mordTariffCode = tariffCode
  , mordFromLocation = fromLocation
  , mordItems = items
  }

makeMinimalYamlOrderRequestData :: YamlOrderRequest -> Int -> SdekFromLocation -> MinimalOrderRequestData
makeMinimalYamlOrderRequestData YamlOrderRequest {..} tariffCode fromLocation =
  let indexedItems = zip [1 ..] yorItems
      items = 
         flip map indexedItems $ \(idx, YamlOrderItem {..}) ->
            OrderItem
            { oiName = yoiName
            , oiArticle = "ART-" <> tshow idx <> "-MAN"
            , oiFabricType = yoiFabricType
            , oiPricePerMetre = yoiPricePerMetre
            , oiTotalPrice = yoiTotalPrice
            , oiLengthM = yoiLengthM
            , oiTelegramUrl = mempty
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
    , mordFromLocation = fromLocation
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
      , pkiPayment = SdekPayment { payValue = 100 }
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
      }
  in
    -- 3. Assemble the final request
    SdekOrderRequest
      { -- Hardcode the tariff for now if you only offer one type of delivery
        sorTariffCode = mordTariffCode -- e.g., "Посылка склад-ПВЗ"
      , sorRecipient = recipient
      , sorPackages = [package]
      , sorFromLocation = mordFromLocation
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
  let ordersReq = getValidSdekToken >>= (_postReq' httpManager ordersUrl order . Just . sdekAccessToken)
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
  let ordersReq = getValidSdekToken >>= (_getReq' httpManager fullUrl mempty . Just . sdekAccessToken)
  eOrders <- makeRequestWithRetries @SdekOrderStatusResponse (Just (void $ getValidSdekToken)) ordersReq
  handleApiResponse @_ @SdekOrderStatusResponse $(currentModule) eOrders $ pure . Right


getOrdersInTransit :: UUID -> AppM (Either HttpError SdekOrderInTransitResponse)
getOrdersInTransit uuid = do
  $(logTM) DebugS $ "Polling SDEK for status of order UUID: " <> ls (UUID.toText uuid)
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let fullUrl = show HTTPS <> url <> "/v2/orders/" <> UUID.toString uuid
  let httpManager = _configHttpManager cfg
  let ordersReq = getValidSdekToken >>= (_getReq' httpManager fullUrl mempty . Just . sdekAccessToken)
  makeRequestWithRetries @SdekOrderInTransitResponse (Just (void $ getValidSdekToken)) ordersReq


scheduleSingleOrderCourier :: (Text, UUID) -> AppM (Either HttpError (Text, SdekCourierResponse))
scheduleSingleOrderCourier (orderId, uuid) = do
  $(logTM) InfoS $ ls $ "scheduling courier for order " <> orderId <> " with SDEK UUID " <> tshow uuid
  today <- liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let courierUrl = show HTTPS <> url <> "/v2/intakes"
  let httpManager = _configHttpManager cfg
  let sdekConfig = _sdekConfig cfg
  let sdekCourierRequest = 
        SdekCallCourierRequest
        { sccrOrderUuid = uuid
        , sccrIntakeDate = tshow today
        , sccrIntakeTimeFrom = Sdek.from (Sdek.pickupWindow sdekConfig)
        , sccrIntakeTimeTo = Sdek.to (Sdek.pickupWindow sdekConfig)
        , sccrSender = SdekCallCourierSender
            { scsName = Sdek.name (Sdek.sender sdekConfig)
            , scsPhones = [SenderPhone (Sdek.phone (Sdek.sender sdekConfig))]
            }
        }
  let courierReq = getValidSdekToken >>= (_postReq' httpManager courierUrl sdekCourierRequest . Just . sdekAccessToken)
  fmap (second (orderId,)) $ makeRequestWithRetries @SdekCourierResponse (Just (void $ getValidSdekToken)) courierReq

getPickupApplicationByUUID :: UUID -> AppM (Either HttpError SdekPickupApplicationResponse)
getPickupApplicationByUUID uuid = do
  $(logTM) DebugS $ "Polling SDEK for pickup application UUID: " <> ls (UUID.toText uuid)
  cfg <-  ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let fullUrl = show HTTPS <> url <> "/v2/intakes/" <> UUID.toString uuid
  let httpManager = _configHttpManager cfg
  let pickupReq = getValidSdekToken >>= (_getReq' httpManager fullUrl mempty . Just . sdekAccessToken)
  makeRequestWithRetries @SdekPickupApplicationResponse (Just (void $ getValidSdekToken)) pickupReq