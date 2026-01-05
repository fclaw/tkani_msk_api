{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Workers.SdekGenerateReceipt (runSdekGenerateReceipt) where

import Katip
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import Control.Monad (forever, void, when)
import Servant.Server (ServerError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import  Data.UUID (UUID)
import GHC.Generics
import qualified Data.ByteString as B
import Control.Concurrent.Async (async)
import Control.Concurrent (threadDelay)
import Data.Maybe (listToMaybe)
import Control.Monad.Reader.Class (ask)
import Network.Wreq (defaults, auth, oauth2Bearer, getWith)
import Control.Lens ((&), (?~))
import Data.Text.Encoding (encodeUtf8)
import Control.Exception (try)
import Network.HTTP.Client (HttpException, responseBody)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)


import App (AppM, extractFromEither, sdekAccessToken, _configHttpManager, ChatKey(ORDER))
import Concurrency (runJobWithCleanup)
import Text (camelToSnake, tshow)
import Infrastructure.Services.Sdek (obtainOrderReceiptUrl)
import Infrastructure.Services.Sdek.Types
import Infrastructure.Services.Sdek.Types.State
import Infrastructure.Services.Sdek.Types.Error
import Infrastructure.Services.Sdek.Auth (getValidSdekToken)
import Infrastructure.Services.Telegram (sendDocument)
import Utils.Telegram.Markdown (escapeMarkdownV2)

-- ADT to parse the notification payload
data ReceiptJob = 
     ReceiptJob 
     { receiptUuid :: UUID
     , orderId :: Text
     , customer :: Text 
     } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake } ''ReceiptJob)

runSdekGenerateReceipt :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runSdekGenerateReceipt connInfo runAppM = do
  $(logTM) InfoS "SDEK Generate Receipt Listener started."
  liftIO $ PG.withConnect connInfo $ \conn -> do
    -- 1. Subscribe to the channel. This must be done on the connection.
    void $ PG.execute_ conn "LISTEN receipt_jobs"
    -- 2. Loop forever, waiting for notifications.
    forever $ do
      -- PG.getNotification blocks until a message arrives.
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      putStrLn $ "Received notification: " <> show payload
      let ePayload = eitherDecode @ReceiptJob $ BL.fromStrict payload
      void $ async $
        -- We still run the main logic inside 'runAppM' to get the AppM context,
        -- but now it's happening in the background.
        void $ runAppM $ runJobWithCleanup (processSingleJob ePayload)

processSingleJob :: Either String ReceiptJob -> AppM ()
processSingleJob (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (ReceiptJob), error: " <> err
processSingleJob (Right ReceiptJob {..}) = do 
  $(logTM) InfoS $ ls $ "Received receipt generator job for order: " <> tshow receiptUuid
  ePollerRes <- getSdekReceipt receiptUuid
  extractFromEither ePollerRes $ \pdfUrl -> do
    $(logTM) InfoS $ "Successfully obtained SDEK receipt URL: " <> ls pdfUrl
    -- Now, download the PDF content
    ePdfContent <- downloadSdekPdf pdfUrl
    case ePdfContent of
      Left err -> $(logTM) ErrorS "Failed to download the receipt PDF."
      Right pdfBytes -> do
        -- 1. We have the file. Now, send it to the order (ORDER) channel.
        todayHashtag <- ((<>) "#t" . T.pack . formatTime defaultTimeLocale "%Y_%m_%d") <$> (liftIO getZonedTime)
        let caption = 
              "📄 Новая квитанция СДЭК для заказа `" <> 
              escapeMarkdownV2 orderId <> 
              "`\n" <> 
              customer <> 
              "\n" <> 
              escapeMarkdownV2 todayHashtag
        let filename = "receipt-" <> orderId <> ".pdf"
        -- 2. Call the new service function
        void $ sendDocument ORDER caption filename pdfBytes "application/pdf"
        $(logTM) InfoS $ "Successfully sent SDEK receipt for " <> ls orderId <> " to admin channel."

-- | Fetches a SDEK receipt PDF link by polling the status endpoint.
--   It will retry several times if the status is 'ACCEPTED' or 'PROCESSING'.
--
--   Returns: The URL to the PDF on success, or an error message on failure.
getSdekReceipt :: UUID -> AppM (Either Text Text)
getSdekReceipt receiptUuid = do
  $(logTM) InfoS $ "Starting to poll for SDEK receipt for order UUID: " <> ls (tshow receiptUuid)
  -- Wrap the entire polling process in a try block to catch any unexpected errors
  pollLoop 1
  where
    maxAttempts = 10 -- Stop polling after 10 tries
    retryDelaySeconds = 5 -- Wait 5 seconds between tries

    -- The recursive polling loop
    pollLoop :: Int -> AppM (Either Text Text)
    pollLoop attempt
      | attempt > maxAttempts = do
          let errMsg = "Polling for SDEK receipt timed out after " <> tshow maxAttempts <> " attempts."
          $(logTM) ErrorS $ ls errMsg
          pure $ Left errMsg

      | otherwise = do
          $(logTM) DebugS $ "Polling for SDEK receipt... Attempt " <> ls (show attempt)
          
          -- 1. Call the SDEK API to get the current status
          eResponse <- obtainOrderReceiptUrl receiptUuid

          case eResponse of
            Left httpErr -> pure $ Left ("HTTP error: " <> tshow httpErr)
            Right response ->
              case listToMaybe (rsrRequests response) of
                Nothing -> pure $ Left "SDEK response was empty."
                Just reqStatus ->
                  case srrState reqStatus of
                    SUCCESSFUL ->
                      -- SUCCESS! The link is available.
                      case reUrl (rsrEntity response) of
                        Just pdfUrl -> do
                          $(logTM) InfoS $ "SDEK receipt is READY. URL: " <> ls pdfUrl
                          pure $ Right pdfUrl
                        Nothing -> pure $ Left "Status is READY but no URL was provided."

                    s | s `elem` [ACCEPTED, WAITING] -> do
                      -- STILL WORKING. Wait and recurse.
                      $(logTM) DebugS $ "SDEK receipt status is " <> ls (show s) <> ". Retrying in " <> ls (show retryDelaySeconds) <> "s..."
                      liftIO $ threadDelay (retryDelaySeconds * 1000000)
                      pollLoop (attempt + 1)

                    INVALID -> do
                      -- PERMANENT FAILURE. Stop polling.
                      let errorMsg = T.intercalate ", " (maybe [] (map message) (srrErrors reqStatus))
                      $(logTM) ErrorS $ "SDEK receipt generation FAILED with status INVALID. Reason: " <> ls errorMsg
                      pure $ Left ("SDEK returned INVALID: " <> errorMsg)
                    otherStatus -> pure $ Left ("Unexpected SDEK receipt status: " <> tshow otherStatus)

-- Helper function to download the file
downloadSdekPdf :: Text -> AppM (Either Text B.ByteString)
downloadSdekPdf pdfUrl = do
  cfg <- ask
  token <- fmap sdekAccessToken getValidSdekToken
  let httpManager = _configHttpManager cfg
--   ... get manager and auth token ...
  let opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 token)
  let handleResp (Left err) = Left (tshow err)
      handleResp (Right response) = Right (BL.toStrict (responseBody response))
  fmap handleResp $ liftIO $ try @HttpException (getWith opts (T.unpack pdfUrl))