{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Workers.PriceCalculator (runPriceCalculator, registerSdekReceipt) where

import Katip hiding (Item)
import Data.Aeson
import Text.Read (readMaybe)
import Data.Int (Int32)
import Data.Aeson.TH
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, listToMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Functor ((<&>))
import Data.UUID (UUID)
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
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
import Network.HTTP.Client (HttpException(..), responseStatus, HttpExceptionContent (..))
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Ord (Down (..))
import Data.List (sortOn)
import Data.Foldable (foldl')
import Data.Bifunctor (second)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
import Network.Wreq (postWith, defaults, manager, responseBody)
import Control.Lens ((&), (.~), (^.))


import API.Types (OrderStatus (Cancelled), Providers (SDEK))
import App (AppM, _configHttpManager, render, _appDBPool, _conciergeBotUrl, _sdekConfig, extractFromMaybe, extractFromEither, ChatKey (ORDER, MAIN), forkAppM, _bots, extractFromEither)
import Text (camelToSnake, tshow)
import TH.Location (currentModule)
import Infrastructure.Database 
       ( setReceiptReady
       , getYamlOrderDetailsForPricing
       , extractValue
       , getPatchedOrderDetails
       , getSdekOrderDetailsForPricing
       , updateOrderStatus
       , fetchOrderDetailsForYaml
       , setDeliveryCost
       , getYandexOrderDetailsForPricing
       , storeYandexOrderParticulars
       , getChatDetails
       , resetOrderDimensionsAndWeight
       , saveYandexPrepaidDeliveryCost
       , PatchedOrderDetails (..)
       , PatchedOrderDetailsItem (..)
       , YandexOrderDetailsForPricing (..))
import Infrastructure.Database.Types (PriceInfo (..), defPriceInfo)
import Infrastructure.Services.Sdek.CachedCityCodes (fetchCityCodeForPvz)
import Infrastructure.Services.Sdek.Types.Config (dropOffPoint, commissionRate)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, sendDocument)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Sdek.Types.Enums (SdekVatRate (VatRate7, NoVat), vatToDouble)
import Infrastructure.Services.Sdek (getTotalSumByTariff, patchOrder, requestReceiptGeneration, getOrderStatus)
import Infrastructure.Services.Sdek.Types
import Concurrency (runJobWithCleanup)
import Workers.SimpleOrderOrchestrator (try', notifyOrderChannelAboutError)
import Infrastructure.Services.Telegram (disableLinkPreviewOption, ParseMode(MarkdownV2))
import Infrastructure.Services.Sdek.Types.State (SdekRequestState (..))
import Infrastructure.Services.Sdek.Types.Error
import Infrastructure.Services.Yandex.Types
import Infrastructure.Services.Yandex.Error (getError, getHttpException)
import Infrastructure.Services.Yandex (calculatePrice, createOrder, fetchParcelLabel, fetchTrackingUrl, fetchPickupPointAddress)
import Infrastructure.Services.Yandex.Order (biDeliveryCost, Item (..), Place (..), PhysicalDimensions (..), ibdAssessedUnitPrice, psPlatformId, drnPlatformStation, srnPlatformStation)


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
               | otherwise = getSdekOrderDetailsForPricing
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
doYandexCalculation orderId = do
  $(logTM) InfoS $ ls $ "Received price calculation job for YANDEX order: " <> orderId
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- getYandexOrderDetailsForPricing orderId pool
  extractValue eDbRes $ \YandexOrderDetailsForPricing {..} -> do
    let resultOrderReq = fromJSON @YandexCreateOrderReq yodpDraftOrderReqJson
    case resultOrderReq of 
      Error err -> $(logTM) ErrorS $ "YandexCreateOrderReq parse failure: " <> ls err
      Success draftOrderReq@YandexCreateOrderReq {..} -> do
        let dimensions = PhysicalDimensions yodpLength yodpWidth yodpHeight yodpWeight
        let pickupId = psPlatformId (fromJust (drnPlatformStation destination))
        let dest = PlatformStationId pickupId
        let src = PlatformStationId (psPlatformId (srnPlatformStation source))
        let priceCalcReq = 
             PriceCalculatorReq
             { pcrTariff             = lastMilePolicy
             , pcrDestination        = dest
             , pcrSource             = src
             , pcrTotalWeight        = yodpWeight
             , pcrPlaces             = [PlacePhysicalDimensions dimensions]
             , pcrTotalAssessedPrice = 
               sum $ items <&> \Item {iBillingDetails} -> 
                 ibdAssessedUnitPrice iBillingDetails
             }
        $(logTM) InfoS $ "PriceCalculatorReq: -> " <> ls (encodePretty priceCalcReq)
        eResCalPrice <- calculatePrice priceCalcReq
        case eResCalPrice of
          Left err -> do
            $(logTM) ErrorS $ "calculatePrice failure: " <> ls (show err)
            void $ resetOrderDimensionsAndWeight orderId pool
            let maybeHttpExcep = getHttpException err
            for_ maybeHttpExcep $ \excep -> do
              let errMsg = escapeMarkdownV2 $ "‼️ " <> getError excep
              void $ sendOrEditTelegramMessage mempty errMsg ORDER Nothing Nothing Nothing
          Right cal@PriceCalculatorResp {..} -> do

            let intPrice = calculateFinalConsumerPrice $ toKopecks pcrPricingTotal

            if yodpIsPrepaid then do
              void $ saveYandexPrepaidDeliveryCost orderId intPrice pool
              sendPrepaidPaymentLink orderId intPrice yodpWeight
            else do
              let orderReq =
                    draftOrderReq {
                      billingInfo = 
                        billingInfo 
                        { biDeliveryCost = intPrice }
                    , places = [Place dimensions orderId]
                    }
              eOrderResp <- createOrder orderReq
              case eOrderResp of
                -- extract error and send to the order channel
                Left err -> do
                  $(logTM) ErrorS $ "createOrder failure: " <> ls (show err)
                  void $ resetOrderDimensionsAndWeight orderId pool
                  let maybeHttpExcep = getHttpException err
                  for_ maybeHttpExcep $ \excep -> do
                    let errMsg = escapeMarkdownV2 $ "‼️ " <> getError excep
                    void $ sendOrEditTelegramMessage mempty errMsg ORDER Nothing Nothing Nothing
                Right resp -> do
                  ePdfContent <- generateParcelLabel (requestId resp)
                  case ePdfContent of
                    Left err ->
                      -- reset dimensions, weight and try again
                      $(logTM) ErrorS $ "Failed to download the receipt PDF." <> ls err
                    Right pdfBytes -> do
                      -- all or nothing 
                      storeYandexOrderParticulars orderId (requestId resp) pdfBytes pool
                      -- 1. We have the file. Now, send it to the order (ORDER) channel.
                      todayHashtag <- ((<>) "#t" . T.pack . formatTime defaultTimeLocale "%Y_%m_%d") <$> (liftIO getZonedTime)
                      let caption = 
                            "📄 Новая квитанция Yandex для заказа `" <> 
                            escapeMarkdownV2 orderId <> 
                            "`\n" <> 
                            yodpCustomer <>
                            "\n" <> 
                            escapeMarkdownV2 todayHashtag
                      let filename = "receipt-" <> orderId <> ".pdf"
                      -- 2. Call the new service function
                      void $ sendDocument ORDER caption filename pdfBytes "application/pdf"
                      $(logTM) InfoS $ "Successfully sent Yandex receipt for " <> ls orderId <> " to admin channel."
                      -- send message about price and tracking number to the telegram channel
                      forkAppM $ sendPriceAndTrackingNumber orderId (requestId resp) pickupId cal


-- | Converts "203.81 RUB" -> 20381
toKopecks :: T.Text -> Int32
toKopecks input = fromMaybe 0 $ do
    -- 1. Take everything before the space ("203.81")
    let pricePart = T.takeWhile (/= ' ') input
    -- 2. Parse as Double to handle the decimal
    val <- readMaybe (T.unpack pricePart) :: Maybe Double
    -- 3. Multiply by 100 and round to Int
    return $ round (val * 100)


-- | Calculates the final total the customer must see on their screen/receipt
-- | to cover the Yandex 3% acquiring fee.
calculateFinalConsumerPrice :: Int32 -> Int32
calculateFinalConsumerPrice deliveryBaseKopecks =
  let 
      subtotal = deliveryBaseKopecks
      -- We multiply by 1.03 to cover the ~3% fee
      feeBuffer = ceiling (fromIntegral subtotal * 0.032 :: Double)
  in 
      subtotal + feeBuffer

-- | 
-- Attempts to generate and fetch the parcel label.
-- Retries on 409 Conflict (Yandex's "Not Ready" state).
generateParcelLabel :: Text -> AppM (Either Text B.ByteString)
generateParcelLabel orderId = go (1 :: Int)
  where
    maxRetries = 5
    baseDelay  = 2000000 -- 2 seconds in microseconds
    go attempt = do
      -- Logic: Try to perform the HTTP call
      result <- fetchParcelLabel orderId
      case result of
        Right pdfBytes -> 
          pure $ Right pdfBytes -- Success: We got the PDF ByteString
        Left (HttpExceptionRequest _ (StatusCodeException resp _)) 
          | statusCode (responseStatus resp) == 409 ->
            if attempt >= maxRetries
            then pure $ Left "Timeout: Label generation still in progress after several retries."
            else do
              -- Log the delay if you have logging set up
              -- logWarn $ "Order not ready (409). Retry #" <> show attempt
              $(logTM) InfoS $ "Order label not ready (409). Retry #" <> ls (show attempt)
              liftIO $ threadDelay (baseDelay * attempt) -- Exponential backoff
              go (attempt + 1)
        -- Handle other unexpected HTTP errors immediately
        Left err -> pure $ Left $ "API call failed: " <> tshow err


sendPriceAndTrackingNumber :: Text -> Text -> Text -> PriceCalculatorResp -> AppM ()
sendPriceAndTrackingNumber orderId yandexOrderId pickupId PriceCalculatorResp {..} = do
  trackingUrl <- fetchTrackingUrl yandexOrderId
  PickupPointAddressResp address <- fetchPickupPointAddress $ PickupPointAddressReq [pickupId]
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- getChatDetails orderId pool
  extractFromEither eDbRes $ \maybeDetails ->
    extractFromMaybe maybeDetails $ \(chatId, _) -> do
      bots <- fmap _bots ask
      let (bot,_) = (M.!) bots MAIN
      let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendMessage"
      let templateData = 
            HM.fromList 
            [ ("deliveryCost", pcrPricingTotal)
            , ("deliveryDays", tshow pcrDeliveryDays)
            , ("orderId",      orderId)
            , ("address",      address)
            ]
      let button = 
            object [
             "inline_keyboard" .=
             [[ object 
              [ "text" .= ("Отследить на сайте YANDEX DELIVERY" :: Text)
              , "url"  .= sharingUrl trackingUrl
              ]
             ]]
            ]
      message <- fmap escapeMarkdownV2 $ render ($currentModule <> ".Yandex") templateData
      let payload =
            object
            [ "chat_id"              .= chatId
            , "text"                 .= message
            , "parse_mode"           .= tshow MarkdownV2
            , "link_preview_options" .= 
                disableLinkPreviewOption
            , "reply_markup"         .= button
            ]
      httpManager <- fmap _configHttpManager ask 
      eTelResp <- liftIO $ try' $ postWith (defaults & manager .~ Right httpManager) url payload
      when (isLeft eTelResp) $ do
        $(logTM) ErrorS $ 
          "telegram failed to deliver message " <> 
          ls (show eTelResp)
        notifyOrderChannelAboutError $ tshow eTelResp

sendPrepaidPaymentLink :: Text -> Int32 -> Int32 -> AppM ()
sendPrepaidPaymentLink orderId price weight = do
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- getChatDetails orderId pool
  extractFromEither eDbRes $ \maybeDetails ->
    extractFromMaybe maybeDetails $ \(chatId, _) -> do
      bots <- fmap _bots ask
      let (bot,_) = (M.!) bots MAIN
      let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendMessage"
      let templateData = 
            HM.fromList 
            [ ("orderId", orderId)
            , ("weight", tshow weight)
            , ("inflatedPrice", tshow (fromIntegral price / 100.0))
            ]
      let botUrl = _conciergeBotUrl cfg
      let deepLinkUrl = botUrl <> "?start=prepaid_" <> orderId
      let button = 
            object [
             "inline_keyboard" .=
             [[ object 
              [ "text" .= ("💳 Получить ссылку на оплату" :: Text)
              , "url"  .= deepLinkUrl
              ]
             ]]
            ]
      message <- fmap escapeMarkdownV2 $ render ($currentModule <> ".Yandex.Prepaid") templateData
      let payload =
            object
            [ "chat_id"              .= chatId
            , "text"                 .= message
            , "parse_mode"           .= tshow MarkdownV2
            , "link_preview_options" .= 
                disableLinkPreviewOption
            , "reply_markup"         .= button
            ]
      httpManager <- fmap _configHttpManager ask
      eTelResp <- liftIO $ try' $ postWith (defaults & manager .~ Right httpManager) url payload
      when (isLeft eTelResp) $ do
        $(logTM) ErrorS $ 
          "telegram failed to deliver message " <> 
          ls (show eTelResp)
        notifyOrderChannelAboutError $ tshow eTelResp