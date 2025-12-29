{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards   #-}

module Handlers.PlaceYamlOrder (handler) where

import Katip (logTM, Severity(..), ls)
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import qualified Data.UUID as UUID
import Data.Text (Text, pack)
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
import App (AppM, SdekJob (..), _appDBPool, _sdekConfig, _appSdekChan, currentTime, render, ChatKey (YAML_ORDER))
import API.Types (ApiResponse, YamlOrderRequest (..), yorItems, mkError, YamlOrderResponse (..))
import qualified Infrastructure.Services.Sdek.Types.Config as Sdek
import qualified Infrastructure.Services.Sdek.Types as Sdek
import qualified Infrastructure.Services.Sdek as Sdek
import Infrastructure.Utils.OrderId (generateOrderId)
import Infrastructure.Database (placeNewYamlOrder, Order (..))
import TH.Location (currentModule)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Utils.Telegram.Markdown (escapeMarkdownV2)


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
  let senderLocation = Sdek.senderLocation sdekConfig
  let fromLocation = 
        Sdek.defSdekFromLocation
        { Sdek.sflAddress = Sdek.address senderLocation
        , Sdek.sflCode = Sdek.cityCode senderLocation
        , Sdek.sflPostCode = Just $ Sdek.postalCode senderLocation
        }
  let shipmentPoint = Sdek.dropOffPoint sdekConfig

  let tariff = yorTariff yamlOrderReq
  when(tariff`notElem` tariffCodes) $ error $ "YamlOrder: tariff not found: " <> show tariff

  let maybeFromLocation | tariff == 138 = Just fromLocation
                        | otherwise = Nothing
  let maybeShipmentPoint | tariff == 136 = Just shipmentPoint
                         | otherwise = Nothing
    
  let requestData = Sdek.makeMinimalYamlOrderRequestData yamlOrderReq tariff maybeFromLocation maybeShipmentPoint      
  eRes <- Sdek.registerOrder $ Sdek.buildMinimalOderRequest requestData
  case eRes of 
    Left err -> pure $ Left $ mkError $ tshow err
    Right trackingUuid -> do 
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
          let yamlDbOrder = mkYamlDbOrder orderId yamlOrderReq trackingUuid trackingNumber
          let mkResponse (Right _) = 
                Right $ YamlOrderResponse 
                { yorOrderId = orderId }
              mkResponse (Left dbErr) = 
                Left $ mkError $ "Failed to store order in DB: " <> dbErr  
          eDbRes <- liftIO $ placeNewYamlOrder yamlDbOrder pool
          when(isLeft eDbRes) $ $(logTM) ErrorS $ ls $ "YamlOrderRequest: db failure " <> show eDbRes
          for_ eDbRes $ const $ do 
            tm <- currentTime
            tz <- liftIO getCurrentTimeZone
            let localTime = utcToLocalTime tz tm
            -- Automatically finds and renders 'templates/Handlers/PlaceNewOrder.tpl'
            messageText <- render $currentModule $ buildTemplateData orderId localTime trackingNumber yamlOrderReq
            void $ sendOrEditTelegramMessage ("new order: " <> orderId) (escapeMarkdownV2 messageText) YAML_ORDER Nothing Nothing Nothing
          return $ mkResponse eDbRes

fetchOrderPollerRes :: UUID.UUID -> AppM (Either Text Text)
fetchOrderPollerRes uuid = do 
  st <- get
  inChan <- fmap _appSdekChan $ liftIO $ atomically $ readTVar st -- The poller's INput chan
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


mkYamlDbOrder :: Text -> YamlOrderRequest -> UUID.UUID -> Text -> Order
mkYamlDbOrder orderId YamlOrderRequest {..} trackingUuid trackingNumber =
  Order 
  { _orderId = orderId
  , _orderCustomerFullName = yorCustomerFullName
  , _orderCustomerPhone =  yorCustomerPhone
  , _orderDeliveryProviderId = encodeToText yorDeliveryProviderId
  , _orderDeliveryPointId = yorDeliveryPointId
  , _orderSdekRequestUuid = trackingUuid
  , _orderSdekTrackingNumber = trackingNumber
  , _orderInternalNotificationMessageId = 0
  , _orderTelegramUserId = 0
  , _orderTariff = fromIntegral yorTariff
  }

-- buildTemplateData :: Text -> LocalTime -> Text -> YamlOrderRequest -> TemplateData
buildTemplateData orderId localTime trackingNumber YamlOrderRequest {..} =
  HM.fromList
  [ ("orderId", orderId)
  , ("timestamp", pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime)
  , ("trackingNumber", trackingNumber)
  , ("customerName", yorCustomerFullName)
  , ("customerPhone", yorCustomerPhone)
  ]