{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RecordWildCards  #-}


module Infrastructure.Services.Sdek 
       ( getDeliveryPoints
       , registerOrder
       , makeMinimalOrderRequestData
       , buildMinimalOderRequest
       , getOrderStatus
       ) where

import Data.Text (Text)
import Control.Monad.IO.Class
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

import App (AppM, sdekAccessToken, _sdekUrl, _pointCache, currentTime)
import API.Types
import Infrastructure.Utils.Http
import Infrastructure.Services.Sdek.Auth (getValidSdekToken)
import TH.Location (currentModule)
import API.WithField (WithField (..))
import Infrastructure.Services.Sdek.CachedDeliveryPoints (storeDeliveryPoints)
import Infrastructure.Services.Sdek.Types
import Data.Maybe (fromMaybe)



getDeliveryPoints :: Text -> AppM (ApiResponse [WithField "dpMetros" [T.Text] DeliveryPoint])
getDeliveryPoints city = do
  url <- fmap (T.unpack . _sdekUrl) ask
  -- Step 1: Find the SDEK city code.
  $(logTM) InfoS $ logStr $ "Fetching SDEK city code for " <> city
  let cityUrl = "https://" <> url <> "/v2/location/cities"
  let cityParams =
        [ ("country_codes", "RU")  -- THE FIX: Limit search to Russia
        , ("city", city)           -- The city name to search for
        , ("size", "1")            -- Optional but good practice: we only need one result
        , ("lang", "rus")          -- Optional but good practice: ensure Russian response
        ]
  let cityReq = getValidSdekToken >>= (_getReq' cityUrl cityParams . Just . sdekAccessToken)
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

    -- | The human-readable name of the fabric being purchased (e.g., "Пальтовый кашемир от Dior").
    --   Source: Bot context, from the product the user initially selected.
  , mordFabricName  :: Text

    -- | The unique article number or SKU for the fabric in our internal system.
    --   This is crucial for SDEK fiscalization and our own database records.
    --   Source: Bot context, from the product the user initially selected.
  , mordWareKey     :: Text

    -- | The final calculated price for the specific cut or piece of fabric.
    --   Source: Bot context, calculated based on length/pre-cut choice.
  , mordFabricPrice :: Double

    -- | The unique code for the chosen SDEK delivery point (e.g., "MSK622").
    --   Note: This should be the code *without* any "sdek_" prefix.
    --   Source: User selection from the paginated list of delivery points.
  , mordDeliveryPointCode :: Text

  , mordTariffCode :: Int
  , mordShipmentPoint :: Text
  }


stripPrefix :: Text -> Text -> Text
stripPrefix prefix txt =
  -- T.stripPrefix is from Data.Text. It returns a 'Maybe Text'.
  -- If the prefix matches, it returns 'Just the_rest_of_the_string'.
  -- If it doesn't match, it returns 'Nothing'.
  case T.stripPrefix prefix txt of
    Just rest -> rest    -- Prefix matched, return the rest of the string.
    Nothing   -> txt     -- Prefix did not match, return the original string.


makeMinimalOrderRequestData :: OrderRequest -> Double -> Int -> Text -> MinimalOrderRequestData
makeMinimalOrderRequestData OrderRequest {..} fabricPrice tariffCode shipmentPoint =
  MinimalOrderRequestData 
  { mordName = orCustomerFullName
  , mordPhone = orCustomerPhone
  , mordFabricName = orFabricName
  , mordWareKey = orArticle
  , mordFabricPrice = fabricPrice
  , mordDeliveryPointCode = fromMaybe orDeliveryPointId (T.stripPrefix "sdek_" orDeliveryPointId)
  , mordTariffCode = tariffCode
  , mordShipmentPoint = shipmentPoint
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
    item = SdekPackageItem
      { pkiName = mordFabricName -- A generic name is fine for your manual workflow
      , pkiWareKey = mordWareKey -- Use your internal fabric ID
      , pkiPayment = SdekPayment { payValue = 100 }
      , pkiWeight = 500 -- A sensible default weight in grams
      , pkiAmount = 1   -- It's one "item" (one piece of fabric)
      , pkiCost = round mordFabricPrice
      }

    -- 3. Create a default package payload
    --    You MUST provide an estimated weight. You can't skip this.
    --    A sensible default (e.g., 500g) is a good start.
    package = SdekPackage
      { pkgNumber = "1" -- Simple default for one-package orders
      , pkgWeight = 1 -- Default weight in grams
      , pkgItems = [item]
      }
  in
    -- 3. Assemble the final request
    SdekOrderRequest
      { -- Hardcode the tariff for now if you only offer one type of delivery
        sorTariffCode = mordTariffCode -- e.g., "Посылка склад-ПВЗ"
      , sorRecipient = recipient
      , sorPackages = [package]
      , sorShipmentPoint = mordShipmentPoint
      , sorDeliveryPoint = mordDeliveryPointCode
      , sorServices = [SdekService INSURANCE (Just (T.pack (show (mordFabricPrice + 1))))]
      }


registerOrder :: SdekOrderRequest -> AppM (Either SdekError UUID)
registerOrder order = do
  url <- fmap (T.unpack . _sdekUrl) ask
  -- Step 1: Find the SDEK city code.
  $(logTM) InfoS $ logStr $ "registering order in sdek" <> show order
  let ordersUrl = "https://" <> url <> "/v2/orders"
  let ordersReq = getValidSdekToken >>= (_postReq' ordersUrl order . Just . sdekAccessToken)
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
  url <- fmap (T.unpack . _sdekUrl) ask
  let fullUrl = "https://" <> url <> "/v2/orders/" <> UUID.toString uuid
  let ordersReq = getValidSdekToken >>= (_getReq' fullUrl mempty . Just . sdekAccessToken)
  eOrders <- makeRequestWithRetries @SdekOrderStatusResponse (Just (void $ getValidSdekToken)) ordersReq
  handleApiResponse @_ @SdekOrderStatusResponse $(currentModule) eOrders $ pure . Right