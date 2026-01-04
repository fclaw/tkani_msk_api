{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workers.SdekPriceCalculator (runSdekPriceCalculator) where

import Katip
import Data.Aeson
import Data.Aeson.TH
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.UUID (UUID)
import qualified Data.ByteString.Lazy as BL
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void, when)
import Servant.Server (ServerError)
import Control.Monad.Reader.Class (ask)
import Control.Concurrent.Async (async)
import Control.Concurrent (threadDelay)
import System.Timeout.Lifted (timeout)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Ord (Down (..))
import Data.List (sortOn)


import App (AppM, _appDBPool, _sdekConfig, extractFromMaybe, extractFromEither)
import Text (camelToSnake, tshow)
import Infrastructure.Database 
       ( setReceiptReady
       , getYamlOrderDetailsForPricing
       , extractValue
       , getPatchedOrderDetails
       , PatchedOrderDetails (..)
       , PatchedOrderDetailsItem (..))
import Infrastructure.Database.Types (PriceInfo (..), defPriceInfo)
import Infrastructure.Services.Sdek.CachedCityCodes (fetchCityCodeForPvz)
import Infrastructure.Services.Sdek.Types.Config (dropOffPoint)
import Infrastructure.Services.Sdek (getTotalSumByTariff, patchOrder, requestReceiptGeneration, getOrderStatus)
import Infrastructure.Services.Sdek.Types
import Concurrency (runJobWithCleanup)
import Infrastructure.Services.Sdek.Types.State (SdekRequestState (..))
import Infrastructure.Services.Sdek.Types.Error


-- ADT to parse the notification payload
data PriceJob = PriceJob { orderId :: Text, isBot :: Bool }
  deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake } ''PriceJob)

getOrderDetailsForPricing _ _ = return $ Left "getOrderDetailsForPricing for bot is not implemented yet"


runSdekPriceCalculator :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runSdekPriceCalculator connInfo runAppM = do 
  $(logTM) InfoS "SDEK Price Calculation Listener started."
  liftIO $ PG.withConnect connInfo $ \conn -> do
    -- 1. Subscribe to the channel. This must be done on the connection.
    void $ PG.execute_ conn "LISTEN price_calculation_jobs"
    -- 2. Loop forever, waiting for notifications.
    forever $ do
      -- PG.getNotification blocks until a message arrives.
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      putStrLn $ "Received notification: " <> show payload
      let ePayload = eitherDecode @PriceJob $ BL.fromStrict payload
      -- === THIS IS THE FIX ===
      -- Fork a new, lightweight thread to do the heavy lifting.
      -- The 'forever' loop can immediately continue to the next 'getNotification'.
      void $ async $
        -- We still run the main logic inside 'runAppM' to get the AppM context,
        -- but now it's happening in the background.
        void $ do threadDelay (5 * 1000000) 
                  runAppM $ runJobWithCleanup (processSingleJob ePayload)

processSingleJob :: Either String PriceJob -> AppM ()
processSingleJob (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (PriceJob), error: " <> err
processSingleJob (Right PriceJob {..}) = do 
  $(logTM) InfoS $ ls $ "Received price calculation job for order: " <> orderId
  cfg <- ask
  let pool = _appDBPool cfg
  let fromPVZ = dropOffPoint $ _sdekConfig cfg
  let dbAction | not isBot = getYamlOrderDetailsForPricing
               | otherwise = getOrderDetailsForPricing
  eDbRes <- dbAction orderId pool
  extractValue eDbRes $ \PriceInfo {..} -> do
    maybeFrom <- fetchCityCodeForPvz fromPVZ
    maybeTo <- fetchCityCodeForPvz piPickUpPoint
    extractFromMaybe maybeFrom $ \from ->
      extractFromMaybe maybeTo $ \to -> do
        let package = 
             Package 
             { pWeight = piWeight
             , pLength = piLength
             , pWidth = piWidth
             , pHeight = piHeight
             }
        let service = TotalSumRequestService "INSURANCE" (tshow piPrice)
        let totalSumReq = mkTotalSumRequest piTariff from to package service
        eResp <- getTotalSumByTariff totalSumReq
        extractFromEither eResp $ \total@TotalSumResponse {..} -> do
          $(logTM) InfoS $ ls $ "TotalSumResponse: \n" <> encodePretty total
          eDbRes <- getPatchedOrderDetails orderId pool
          extractValue eDbRes $ \details@PatchedOrderDetails {..} -> do
            case tsrErrors of 
              Just errs -> $(logTM) ErrorS $ ls $ "getPatchedOrderDetails has resulted in error: " <> show errs
              Nothing -> do
                $(logTM) InfoS $ ls $ "PatchedOrderDetails: \n" <> encodePretty details
                let totalCost = fromMaybe undefined tsrTotalSum
                let items = flip map (zip [1..] podItems) $ \(idx, PatchedOrderDetailsItem {..}) -> 
                            PatchedOrderRequestPackageItem
                            { porpiName    = podiName 
                            , porpiWareKey = podiArticle
                            , porpiPayment = SdekPayment $ 
                                if idx == 1 then 
                                  addFivePercent totalCost 
                                else 0 
                            , porpiWeight  = podiWeight
                            , porpiAmount  = 1
                            , porpiCost    = podiCost
                            }
                let package = defPatchedOrderRequestPackage 
                              { porpWeight = podParcelWeight
                              , porpLength = podLength
                              , porpWidth  = podWidth
                              , porpHeight = podHeight
                              , porpItems  = items
                              }
                let patchedOrderReq = PatchedOrderRequest podSdekUuid [package]
                $(logTM) InfoS $ ls $ "PatchedOrderRequest: \n" <> encodePretty patchedOrderReq
                eResp <- patchOrder patchedOrderReq
                extractFromEither eResp $ \resp@PatchedOrderResponse {..} ->
                  case porRequests of 
                   [] -> $(logTM) ErrorS $ "porRequests field is empty in PatchedOrderResponse"
                   (req:_) -> 
                     case porbState req of
                       Accepted -> do
                         $(logTM) InfoS $ "SDEK order patch ACCEPTED. Polling for completion..."
                         ePollRes <- pollForPatchCompletion podSdekUuid
                         case ePollRes of
                           Left pollErr -> $(logTM) ErrorS $ "Polling for patch completion failed for order " <> ls orderId <> ": " <> ls pollErr
                           Right finalOrderState -> do
                             -- NOW it is safe to generate the receipt
                             $(logTM) InfoS $ "SDEK order patch is complete. Requesting receipt generation..."
                             $(logTM) InfoS $ ls $ 
                               "request for a receipt to be printed, order " <> 
                               orderId <> 
                               ", sdek uuid: " <> 
                               tshow podSdekUuid
                             eReqReq <- registerReceipt podSdekUuid
                             extractFromEither eReqReq $ \receiptUuid -> do
                               eDbRes <- setReceiptReady orderId receiptUuid pool 
                               extractValue eDbRes $ \_ -> 
                                 $(logTM) InfoS $ ls $ "order " <> orderId <> " has been successfully patched"
                               when(isLeft eDbRes) $ $(logTM) ErrorS $ ls $ "order " <> orderId <> ", db failure " <> tshow eDbRes
                       state -> $(logTM) ErrorS $ ls $ "order " <> orderId <> " is not patched, state: " <> tshow state    
                       

addFivePercent cost = cost + cost * 0.05

-- requestReceiptGeneration
registerReceipt :: UUID -> AppM (Either Text UUID)
registerReceipt sdekUuid = do
  $(logTM) InfoS $ "Starting SDEK receipt generation process for order UUID: " <> ls (show sdekUuid)        
  eResponse <- requestReceiptGeneration sdekUuid
  case eResponse of
    Left httpErr -> pure $ Left ("HTTP error during polling: " <> tshow httpErr)
    Right response ->
      case rrrState response of
        s | s `elem` [Successful, Accepted, Waiting] ->
          pure $ Right $ rrrUuid response
        Invalid -> do
          let errorMsg = T.intercalate ", " (maybe [] (map message) (rrrErrors response))
          pure $ Left ("SDEK returned INVALID: " <> errorMsg)
        other -> pure $ Left ("Unexpected SDEK receipt status: " <> tshow other)

-- | Polls an order's status until it is no longer in an 'ACCEPTED' state,
--   confirming that a PATCH or other update has been fully processed.
pollForPatchCompletion :: UUID -> AppM (Either Text SdekRequestState)
pollForPatchCompletion orderUuid = go 1
  where
    maxAttempts = 10
    delaySeconds = 3

    go attempt
      | attempt > maxAttempts = pure $ Left "Timed out waiting for SDEK order patch to complete."
      | otherwise = do
          eStatusResponse <- getOrderStatus orderUuid -- Your GET /v2/orders/{uuid} function
          case eStatusResponse of
            Left err -> pure $ Left ("API error during polling: " <> tshow err)
            Right statusResponse ->
                  -- Get the latest status from the history
              let latestStatus = listToMaybe $ sortOn (Down . spsDateTime) (sosrRequests statusResponse)
              in case latestStatus of
                   Just s | spsState s == Accepted -> do
                    -- The PATCH is still being processed. Wait and recurse.
                     $(logTM) DebugS $ "Order " <> ls (tshow orderUuid) <> " is still ACCEPTED. Waiting..."
                     liftIO $ threadDelay (delaySeconds * 1000000)
                     go (attempt + 1)
                        
                   Just s -> do
                     -- Any other status (e.g., CREATED, RECEIVED) means the patch is done.
                     $(logTM) InfoS $ 
                       "SDEK order patch for " <> 
                       ls (tshow orderUuid) <> 
                       " completed with status: " <> 
                       ls (show (spsState s))
                     pure $ Right $ spsState s

                   Nothing -> pure $ Left "SDEK order has no status history."