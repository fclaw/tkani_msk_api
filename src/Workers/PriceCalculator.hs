{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workers.PriceCalculator (runPriceCalculator, registerSdekReceipt) where

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
import qualified Data.Yaml as Yaml
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
import Data.Foldable (foldl')
import Data.Bifunctor (second)


import API.Types (OrderStatus (Cancelled), Providers (SDEK))
import App (AppM, _appDBPool, _sdekConfig, extractFromMaybe, extractFromEither, ChatKey (ORDER))
import Text (camelToSnake, tshow)
import Infrastructure.Database 
       ( setReceiptReady
       , getYamlOrderDetailsForPricing
       , extractValue
       , getPatchedOrderDetails
       , getOrderDetailsForPricing
       , updateOrderStatus
       , fetchOrderDetailsForYaml
       , setDeliveryCost
       , PatchedOrderDetails (..)
       , PatchedOrderDetailsItem (..))
import Infrastructure.Database.Types (PriceInfo (..), defPriceInfo)
import Infrastructure.Services.Sdek.CachedCityCodes (fetchCityCodeForPvz)
import Infrastructure.Services.Sdek.Types.Config (dropOffPoint, commissionRate)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, sendDocument)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Sdek.Types.Enums (SdekVatRate (VatRate7, NoVat), vatToDouble)
import Infrastructure.Services.Sdek (getTotalSumByTariff, patchOrder, requestReceiptGeneration, getOrderStatus)
import Infrastructure.Services.Sdek.Types
import Concurrency (runJobWithCleanup)
import Infrastructure.Services.Sdek.Types.State (SdekRequestState (..))
import Infrastructure.Services.Sdek.Types.Error


-- ADT to parse the notification payload
data PriceJob = 
     PriceJob 
     { orderId  :: Text
     , isBot    :: Bool
     , provider :: Providers 
     }
  deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake } ''PriceJob)


runPriceCalculator :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runPriceCalculator connInfo appMToHandler = do 
  $(logTM) InfoS "Price Calculation Listener started."
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
        -- We still run the main logic inside 'appMToHandler' to get the AppM context,
        -- but now it's happening in the background.
        void $ appMToHandler $ runJobWithCleanup (processSingleJob ePayload)

processSingleJob :: Either String PriceJob -> AppM ()
processSingleJob (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (PriceJob), error: " <> err
processSingleJob (Right PriceJob {..}) 
  | provider == SDEK = doSdekCalculation orderId isBot
  | otherwise = doYandexCalculation orderId -- Placeholder for future Yandex implementation

doSdekCalculation :: Text -> Bool -> AppM ()
doSdekCalculation orderId isBot = do
  $(logTM) InfoS $ ls $ "Received price calculation job for SDEK order: " <> orderId
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
        let service = TotalSumRequestService INSURANCE (tshow piPrice)
        let totalSumReq = mkTotalSumRequest piTariff from to package service
        eResp <- getTotalSumByTariff totalSumReq
        extractFromEither eResp $ \totalSumResponse@TotalSumResponse {..} -> do
          $(logTM) InfoS $ ls $ "TotalSumResponse: \n" <> encodePretty totalSumResponse

          -- validate the total with manually calculated grand total
          let grandTotal = calculateGrandTotal totalSumResponse
          
          when (abs (tsrTotalSum - grandTotal) > 0.01) $ do -- Use a small epsilon for float comparison
            $(logTM) WarningS $
              "SDEK total_sum mismatch! API: " <> 
              ls (show tsrTotalSum) <>
              ", Manual Calc: " <>
              ls (show grandTotal)
            let msg = escapeMarkdownV2 $
                       "SDEK total_sum mismatch! API: " <>
                        tshow tsrTotalSum <> 
                        ", Manual Calc: " <>
                        tshow grandTotal
            void $ sendOrEditTelegramMessage mempty msg ORDER Nothing Nothing Nothing

          eDbRes <- getPatchedOrderDetails orderId pool
          extractValue eDbRes $ \details@PatchedOrderDetails {..} -> do
            case tsrErrors of 
              Just errs -> $(logTM) ErrorS $ ls $ "getPatchedOrderDetails has resulted in error: " <> show errs
              Nothing -> do
                $(logTM) InfoS $ ls $ "PatchedOrderDetails: \n" <> encodePretty details
                -- 1. Calculate the final, VAT-inclusive grand total using your helper
                -- let grandTotalInclusive = calculateGrandTotal totalSumResponse
                let items = flip map (zip [1..] podItems) $ \(idx, PatchedOrderDetailsItem {..}) ->
                      let 
                          -- Define the VAT rate (as a decimal and a divisor)
                          vatRate = vatToDouble VatRate7 / 100.0
                          vatDivisor = 1 + vatToDouble VatRate7 / 100.0

                          -- Back-calculate the VAT sum from the inclusive total.
                          -- This is the correct way to find the tax portion of a final price.
                          totalSumWithCommission = tsrTotalSum * commissionRate (_sdekConfig cfg)
                          baseAmount = totalSumWithCommission / vatDivisor
                          vatAmount = totalSumWithCommission - baseAmount

                      in
                          -- Build the payment object for the FIRST item
                          -- (The payment for all other items will be 0)
                          let paymentDetails = 
                                if idx == 1 -- Use index 0 for the first item
                                then
                                  SdekPayment
                                  { -- This is the final amount the customer pays + agent's commission ~ 3 percent
                                    payValue = totalSumWithCommission,
                                    -- This is the portion of 'payValue' that is VAT
                                    vatSum = Just vatAmount,
                                    -- This tells the fiscal system the rate used
                                    vatRate = Just VatRate7
                                  }
                                else
                                  -- For all other items, the payment is zero
                                  SdekPayment
                                  { payValue = 0.0
                                  , vatSum = Nothing
                                  , vatRate = Just NoVat -- It's good practice to specify "NoVat"
                                  }

                          in PatchedOrderRequestPackageItem
                             { porpiName    = podiName 
                             , porpiWareKey = podiArticle
                             , porpiPayment = paymentDetails                           
                             , porpiWeight  = podiWeight
                             , porpiAmount  = 1
                             , porpiCost    = podiCost
                             }
                let package = defPatchedOrderRequestPackage 
                              { porpWeight = piWeight
                              , porpLength = piLength
                              , porpWidth  = piWidth
                              , porpHeight = piHeight
                              , porpItems  = items
                              , porpNumber = orderId
                              }
                void $ setDeliveryCost orderId (round (tsrTotalSum * commissionRate (_sdekConfig cfg))) pool
                let patchedOrderReq = PatchedOrderRequest podSdekUuid [package]
                $(logTM) InfoS $ ls $ "PatchedOrderRequest: \n" <> encodePretty patchedOrderReq
                eResp <- patchOrder patchedOrderReq
                extractFromEither eResp $ \resp@PatchedOrderResponse {..} ->
                  case porRequests of 
                   [] -> do 
                     $(logTM) ErrorS $ "porRequests field is empty in PatchedOrderResponse"
                     cancelOrder orderId
                   (req:_) -> 
                     case porbState req of
                       Accepted -> do
                         $(logTM) InfoS $ "SDEK order patch ACCEPTED. Polling for completion..."
                         ePollRes <- pollForPatchCompletion podSdekUuid
                         case ePollRes of
                           Left pollErr -> do 
                             $(logTM) ErrorS $ "Polling for patch completion failed for order " <> ls orderId <> ": " <> ls pollErr
                             cancelOrder orderId
                           Right finalOrderState -> do
                             -- NOW it is safe to generate the receipt
                             $(logTM) InfoS $ "SDEK order patch is complete. Requesting receipt generation..."
                             $(logTM) InfoS $ ls $ 
                               "request for a receipt to be printed, order " <> 
                               orderId <> 
                               ", sdek uuid: " <> 
                               tshow podSdekUuid
                             eReqReq <- registerSdekReceipt podSdekUuid
                             extractFromEither eReqReq $ \receiptUuid -> do
                               eDbRes <- setReceiptReady orderId receiptUuid pool
                               extractValue eDbRes $ \_ -> 
                                 $(logTM) InfoS $ ls $ "order " <> orderId <> " has been successfully patched"
                               when(isLeft eDbRes) $ $(logTM) ErrorS $ ls $ "order " <> orderId <> ", db failure " <> tshow eDbRes
                       state -> do 
                         $(logTM) ErrorS $ ls $ "order " <> orderId <> " is not patched, state: " <> tshow state
                         cancelOrder orderId
                       

-- | Calculates the grand total from a SDEK 'TotalSumResponse' by summing the
--   individual components. This is useful for verifying the top-level 'total_sum'.
--
--   The logic is: Grand Total = Base Delivery Cost (delivery_sum) + Sum of all Service Costs.
--   NOTE: This assumes 'delivery_sum' is the pre-tax base, and the 'services'
--   array contains the tax for delivery as a separate line item.
calculateGrandTotal :: TotalSumResponse -> Double
calculateGrandTotal TotalSumResponse {..} =
    -- The Grand Total is the main tariff's inclusive cost PLUS
    -- the inclusive cost of all additional services.

        -- 1. 'tsrTotalSum' is the final, inclusive cost of the base tariff.
    let baseTariffTotal = tsrDeliverySum

        vatRate = vatToDouble VatRate7 / 100.0

        -- 2. 'tsrServices' is a list of *additional* services. We sum their inclusive totals.
        additionalServicesTotal = foldl' (\acc service -> acc + tssTotalSum service) 0.0 tsrServices

    in (baseTariffTotal + vatRate * baseTariffTotal) + additionalServicesTotal


-- requestReceiptGeneration
registerSdekReceipt :: UUID -> AppM (Either Text UUID)
registerSdekReceipt sdekUuid = do
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
                        
                   Just s | spsState s == Invalid -> pure $ Left $ "SDEK order has failed to patch." <> tshow statusResponse

                   Just s -> do
                     -- Any other status (e.g., CREATED, RECEIVED) means the patch is done.
                     $(logTM) InfoS $ 
                       "SDEK order patch for " <> 
                       ls (tshow orderUuid) <> 
                       " completed with status: " <> 
                       ls (show (spsState s))
                     pure $ Right $ spsState s

                   Nothing -> pure $ Left "SDEK order has no status history."

cancelOrder :: Text -> AppM ()
cancelOrder orderId = do
  $(logTM) InfoS $ "Starting SDEK order cancellation process for order: " <> ls orderId
  void $ fmap _appDBPool ask >>= updateOrderStatus orderId Cancelled
  -- make yaml file from order details for manual uploading
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- fetchOrderDetailsForYaml orderId pool
  for_ eDbRes $ \orderDetails -> do
    let bytes = Yaml.encode $ toJSON orderDetails
    let message = escapeMarkdownV2 $ "‼️ order " <> orderId <> " patch fails"
    let file = orderId <> ".yaml"
    void $ sendDocument ORDER message file bytes "application/x-yaml"
  when (isLeft eDbRes) $ $(logTM) ErrorS $ ls $ "fetchOrderDetailsForYaml resulted in error: " <> tshow eDbRes
  

doYandexCalculation :: Text -> AppM ()
doYandexCalculation orderId = $(logTM) WarningS $ ls $ "Received price calculation job for unsupported provider (Yandex): " <> orderId
