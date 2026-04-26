{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Workers.YandexPrepaidOrderRegistrar (runYandexPrepaidOrderRegistrar, registerOrder) where


import Katip
import Data.Aeson (Result (..), fromJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Servant.Server (ServerError)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import qualified Database.PostgreSQL.Simple as PG
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void)
import Control.Concurrent.Async (async)
import Control.Monad.Reader.Class (ask)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Database.PostgreSQL.Simple.Notification as PG


import Text (tshow)
import App (AppM, ChatKey (PREPAID_ORDER, MONEY_TRANSFER), _appDBPool, _bankAccount, _yandexConfig)
import Concurrency (runJobWithCleanup)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Workers.PriceCalculator (finalizeYandexOrder)
import Infrastructure.Services.Yandex (createOrder)
import qualified Infrastructure.Services.Yandex.Config as Ya
import Infrastructure.Services.Tinkoff (initiateTinkoffRubleTransfer)
import Infrastructure.Services.Tinkoff.Types.RubleTransfer
import Infrastructure.Services.Yandex.Types (YandexCreateOrderReq (..), PriceCalculatorResp (..))
import Infrastructure.Database (getYandexOrderDetailsForPricing, extractValue, YandexOrderDetailsForPricing (..))
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Infrastructure.Services.Yandex.Error (getError, getHttpException)
import Infrastructure.Services.Yandex.Order (PhysicalDimensions (..), psPlatformId, drnPlatformStation, Place (..))


data ShipmentPaymentEvent = 
     ShipmentPaymentEvent
     { parcel_order_id :: Text
     , amount          :: Int
     , days            :: Int
     } deriving (Show, Generic)

instance FromJSON ShipmentPaymentEvent -- Aeson decodes the JSONB from Postgres


runYandexPrepaidOrderRegistrar ::  PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runYandexPrepaidOrderRegistrar connInfo appMToHandler = do
  $(logTM) InfoS "Yandex prepaid order registrar Listener started."
  liftIO $ PG.withConnect connInfo $ \conn -> do
    -- 1. Subscribe to the channel. This must be done on the connection.
    void $ PG.execute_ conn "LISTEN shipment_payment_events"
    -- 2. Loop forever, waiting for notifications.
    forever $ do
      -- PG.getNotification blocks until a message arrives.
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      putStrLn $ "Received notification: " <> show payload
      let ePayload = eitherDecode @ShipmentPaymentEvent $ BL.fromStrict payload
      -- === THIS IS THE FIX ===
      -- Fork a new, lightweight thread to do the heavy lifting.
      -- The 'forever' loop can immediately continue to the next 'getNotification'.
      void $ async $
        -- We still run the main logic inside 'appMToHandler' to get the AppM context,
        -- but now it's happening in the background.
        void $ appMToHandler $ runJobWithCleanup (processSingleEvent ePayload)

processSingleEvent :: Either String ShipmentPaymentEvent -> AppM ()
processSingleEvent (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (ShipmentPaymentEvent), error: " <> err
processSingleEvent (Right ShipmentPaymentEvent {..}) = registerOrder parcel_order_id amount days

registerOrder :: Text -> Int -> Int -> AppM ()
registerOrder orderId amount days = do
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
        let orderReq = draftOrderReq { places = [Place dimensions orderId] }
        eOrderResp <- createOrder orderReq
        case eOrderResp of
          -- extract error and send to the order channel
          Left err -> do
            $(logTM) ErrorS $ "createOrder failure (prepaid): " <> ls (show err) <> ", order: " <> ls orderId
            let maybeHttpExcep = getHttpException err
            for_ maybeHttpExcep $ \excep -> do
              let errMsg = escapeMarkdownV2 $ "‼️ " <> getError excep <> ", order: " <> orderId
              void $ sendOrEditTelegramMessage mempty errMsg PREPAID_ORDER Nothing Nothing Nothing
          Right resp -> do
            let msg = 
                 "✅ yandex shipment for order " <> 
                 orderId <>
                 " has been confirmed. amount: " <>
                 tshow (fromRational (fromIntegral amount) / 100.0)
            void $ sendOrEditTelegramMessage mempty (escapeMarkdownV2 msg) PREPAID_ORDER Nothing Nothing Nothing
            let cal = PriceCalculatorResp (fromIntegral days) "0"
            finalizeYandexOrder orderId pickupId resp cal yodpCustomer

            -- transfer money from our bank account to the Yandex settlement account
            transferReqId <- fmap (T.pack . toString) $ liftIO nextRandom
            let yandexCfg = _yandexConfig cfg
            let _amount = fromRational (fromIntegral amount) / 100.0
            let defTransferReq =
                  RubleTransferRequest 
                  { rtId      = transferReqId
                  , rtFrom    = Payer $ _bankAccount cfg
                  , rtTo      = Ya.receiver yandexCfg
                  , rtPurpose = Ya.purpose yandexCfg
                  , rtAmount  = _amount
                  }

            moneyTransferResp <- initiateTinkoffRubleTransfer defTransferReq
            
            let send msg = void $ sendOrEditTelegramMessage mempty (escapeMarkdownV2 msg) MONEY_TRANSFER Nothing Nothing Nothing

            case moneyTransferResp of
              Left err -> send $ tshow err
              Right RubleTransferResponse { rtrError } ->
                case rtrError of
                  Nothing -> send $ 
                    "✅ nmoney of " <> 
                    tshow _amount <> 
                    " transfer initiated to Yandex \
                    \ settlement account for order " <>
                    orderId
                  Just err -> send $ "‼️ \n" <> decodeUtf8 (BL.toStrict (encodePretty err))

