{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module API.Handlers.PlaceYamlOrder (handler) where

import Katip (logTM, Severity(..), ls)
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import qualified Data.UUID as UUID
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Bifunctor (bimap)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, takeTMVar)
import Control.Concurrent.STM (writeTChan, atomically, readTVar)
import System.Timeout (timeout)
import Control.Monad.State.Class (get)
import Control.Monad (void, when)
import Data.Foldable (for_)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Either (isLeft)


import Text (tshow, encodeToText)
import TH.Location (currentModule)
import App (AppM, SdekJob (..), _appDBPool, _sdekConfig, _sdekOrderChan, currentTime, render, ChatKey (YAML_ORDER))
import API.Types (ApiResponse, YamlOrderRequest (..), yorItems, mkError, YamlOrderResponse (..), OrderStatus (Paid), PhysicalDimensions (..), yoiWeight)
import qualified Infrastructure.Services.Sdek.Types.Config as Sdek
import qualified Infrastructure.Services.Sdek.Types as Sdek
import qualified Infrastructure.Services.Sdek as Sdek
import Infrastructure.Utils.OrderId (generateOrderId)
import Infrastructure.Database (placeNewYamlOrder, YamlOrder (..))
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Sdek.CachedTariffs (getTariffs)


handler :: YamlOrderRequest -> AppM (ApiResponse YamlOrderResponse)
handler (YamlOrderRequest { yorItems }) | null yorItems = do
  $(logTM) ErrorS "Received empty YAML order request."
  return $ Left $ mkError "Order must contain at least one item."
handler yamlOrderReq = do
  $(logTM) InfoS $ ls $ "Received YAML order request: " <> show yamlOrderReq
  -- Here you would add the logic to process the YAML order request,
  -- such as validating the data and storing it in the database.
  -- For now, we just log and return a success response.
  cfg <- ask
  let pool = _appDBPool cfg
  let sdekConfig = _sdekConfig cfg
  let tariffCodes =  Sdek.tariffs sdekConfig
  let shipmentPoint = Sdek.dropOffPoint sdekConfig
        
  eRes <- tryTariffs yamlOrderReq shipmentPoint tariffCodes
  case eRes of 
    Left err -> pure $ Left $ mkError $ tshow err
    Right (trackingUuid, tariff) -> do 
      $(logTM) InfoS $ "SDEK request accepted. Waiting for final confirmation for UUID: " <> ls (UUID.toText trackingUuid)
      -- This is the action for our background poller thread.
      $(logTM) InfoS $ "poller tries calling sdek for the final confirmation"
      ePollerRes <- fetchOrderPollerRes trackingUuid
      case ePollerRes of 
        Left pollErr -> do 
          $(logTM) ErrorS $ "Failed to fetch final confirmation from SDEK for UUID: " <> ls (UUID.toText trackingUuid) <> ", error: " <> ls pollErr
          pure $ Left $ mkError $ "Failed to confirm order with SDEK: " <> pollErr
        Right trackingNumber -> do 
          $(logTM) InfoS $ "Successfully received tracking number from SDEK: " <> ls trackingNumber
          -- Now we can store the order in our database.
          orderId <- liftIO $ generateOrderId
          let yamlDbOrder = mkYamlDbOrder orderId yamlOrderReq trackingUuid trackingNumber tariff
          let mkResponse (Right _) = 
                Right $ YamlOrderResponse 
                { yorOrderId = orderId }
              mkResponse (Left dbErr) = 
                Left $ mkError $ "Failed to store order in DB: " <> dbErr  
          eDbRes <- placeNewYamlOrder yamlDbOrder (yorItems yamlOrderReq) pool
          when(isLeft eDbRes) $ $(logTM) ErrorS $ ls $ "YamlOrderRequest: db failure " <> show eDbRes
          for_ eDbRes $ const $ do 
            tm <- currentTime
            tz <- liftIO getCurrentTimeZone
            let localTime = utcToLocalTime tz tm
            -- Automatically finds and renders 'templates/Handlers/PlaceNewOrder.tpl'
            messageText <- render $currentModule $ buildTemplateData orderId localTime trackingNumber yamlOrderReq
            void $ sendOrEditTelegramMessage ("new order from yaml: " <> orderId) (escapeMarkdownV2 messageText) YAML_ORDER Nothing Nothing Nothing
          return $ mkResponse eDbRes

fetchOrderPollerRes :: UUID.UUID -> AppM (Either Text Text)
fetchOrderPollerRes uuid = do 
  st <- get
  inChan <- fmap _sdekOrderChan $ liftIO $ atomically $ readTVar st -- The poller's INput chan
  -- 1. Create a new, empty TMVar for the reply
  replyVar <- liftIO newEmptyTMVarIO
  -- 2. Create the job and put it on the poller's queue
  let job = SdekJob uuid replyVar
  liftIO $ atomically $ writeTChan inChan job
  -- 3. Block and wait for the result to appear in our reply box
  -- We use a timeout to prevent waiting forever.
  let handleRes Nothing = Left "Sdek timeout"
      handleRes (Just res) = res
  fmap handleRes $ liftIO $ timeout (30 * 1000000) $ atomically $ takeTMVar replyVar


mkYamlDbOrder :: Text -> YamlOrderRequest -> UUID.UUID -> Text -> Int -> YamlOrder
mkYamlDbOrder orderId YamlOrderRequest {..} trackingUuid trackingNumber tariff =
  YamlOrder 
  { _yamlOrderId = orderId
  , _yamlOrderCustomerFullName = yorCustomerFullName
  , _yamlOrderCustomerPhone =  yorCustomerPhone
  , _yamlOrderDeliveryProviderId = encodeToText yorDeliveryProviderId
  , _yamlOrderDeliveryPointId = yorDeliveryPointId
  , _yamlOrderSdekRequestUuid = trackingUuid
  , _yamlOrderSdekTrackingNumber = trackingNumber
  , _yamlOrderTariff = fromIntegral tariff
  , _yamlOrderWeight = sum (map (fromIntegral . yoiWeight) yorItems) + 50        
  , _yamlOrderLength = fromIntegral $ pdWidth yorPhysicalDimensions
  , _yamlOrderWidth = fromIntegral $ pdLength yorPhysicalDimensions
  , _yamlOrderHeight = fromIntegral $ pdHeight yorPhysicalDimensions
  }

-- buildTemplateData :: Text -> LocalTime -> Text -> YamlOrderRequest -> TemplateData
buildTemplateData orderId localTime trackingNumber YamlOrderRequest {..} =
  let tm = pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
  in HM.fromList
     [ ("orderId", orderId)
     , ("timestamp", tm)
     , ("trackingNumber", trackingNumber)
     , ("customerName", yorCustomerFullName)
     , ("customerPhone", yorCustomerPhone)
     ]


tryTariffs :: YamlOrderRequest -> Text -> [Sdek.Tariff] -> AppM (Either Text (UUID.UUID, Int))
tryTariffs request shipmentPoint tariffs = do
  eSdekResp <- getTariffs shipmentPoint (fromMaybe undefined (T.stripPrefix "sdek_" (yorDeliveryPointId request)))
  case eSdekResp of
    Left err -> pure (Left (tshow err))
    Right Nothing -> pure $ Left "one of points hasn't been found"
    Right (Just availableTariffs) -> do
      let optimalTariff = Sdek.findOptimalTariff tariffs availableTariffs
      let requestData = Sdek.makeMinimalYamlOrderRequestData request optimalTariff (Just shipmentPoint)
      fmap (bimap tshow (, optimalTariff)) $ Sdek.registerOrder $ Sdek.buildMinimalOderRequest requestData