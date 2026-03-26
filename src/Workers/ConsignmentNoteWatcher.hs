{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Workers.ConsignmentNoteWatcher (runConsignmentNoteWatcher) where

import Katip
import Data.Int (Int64)
import Data.Text (Text)
import Data.Either (isLeft)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, eitherDecode)
import Servant.Server (ServerError)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void, when)
import qualified Data.Map.Strict as M
import Control.Concurrent.Async (async)
import Control.Monad.Reader.Class (ask)
import qualified Network.Wreq as W
import Control.Concurrent (threadDelay)
import Control.Lens ((&), (.~), (^.), (?~))
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Simple.Notification as PG

import Text (tshow)
import App (AppM, _appDBPool, ChatKey (MAIN), _bots, _configHttpManager)
import Concurrency (runJobWithCleanup)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (ParseMode (MarkdownV2), try')
import Infrastructure.Database (fetchConsignmentPdfItems)
import Infrastructure.Services.PdfGenerator (generateConsignmentPdf)


data ConsignmentNoteEvent = 
     ConsignmentNoteEvent
     { order_id :: Text
     } deriving (Show, Generic)

instance FromJSON ConsignmentNoteEvent -- Aeson decodes the JSONB from Postgres

runConsignmentNoteWatcher :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runConsignmentNoteWatcher connInfo appMToHandler = do
  $(logTM) InfoS "Consignment note watcher started."
  liftIO $ PG.withConnect connInfo $ \conn -> do
    -- 1. Subscribe to the channel. This must be done on the connection.
    void $ PG.execute_ conn "LISTEN consignment_note_events"
    -- 2. Loop forever, waiting for notifications.
    forever $ do
      -- PG.getNotification blocks until a message arrives.
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      putStrLn $ "Received notification: " <> show payload
      let ePayload = eitherDecode @ConsignmentNoteEvent $ BL.fromStrict payload
      -- === THIS IS THE FIX ===
      -- Fork a new, lightweight thread to do the heavy lifting.
      -- The 'forever' loop can immediately continue to the next 'getNotification'.
      void $ async $
        -- We still run the main logic inside 'appMToHandler' to get the AppM context,
        -- but now it's happening in the background.
        void $ appMToHandler $ runJobWithCleanup (processSingleEvent ePayload)

    
processSingleEvent :: Either String ConsignmentNoteEvent -> AppM ()
processSingleEvent (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (ConsignmentNoteEvent), error: " <> err
processSingleEvent (Right ConsignmentNoteEvent {..}) = do
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- attemptFetchWithRetry 3 1000000 order_id pool
  case eDbRes of
    Left err -> 
      $(logTM) ErrorS $ 
        "fetchConsignmentPdfItems: \
        \ db failure " <> ls err
    Right Nothing -> $(logTM) ErrorS $ "Consignment failed: 0 items for order " <> ls order_id
    Right (Just (chatId, items)) -> do
      ePdf <- generateConsignmentPdf order_id items
      case ePdf of
        Left err -> 
          $(logTM) ErrorS $ 
            "pdf generation failure " <>
            ls err
        Right pdfBytes -> do
          let filename = "consignment_note-" <> order_id <> ".pdf"
          let caption = "📄 Товарная накладная для заказа " <> order_id
          --  Call the new service function
          sendDocument chatId (escapeMarkdownV2 caption) filename pdfBytes "application/pdf"
        

attemptFetchWithRetry 0 _ _ _                      = pure $ Right Nothing
attemptFetchWithRetry retries delayMicros oid pool = do
  -- Attempt 1 with retry logic
  eDbRes <- fetchConsignmentPdfItems oid pool
  case eDbRes of
    Left err          -> pure $ Left err
    Right val  ->
      if fst val /= 0 then
        pure $ Right $ Just $ val
      else do          
        liftIO $ threadDelay delayMicros
        attemptFetchWithRetry (retries - 1) delayMicros oid pool
 

sendDocument :: Int64 -> Text -> Text -> B.ByteString -> Text -> AppM ()
sendDocument chatId caption filename pdfBytes mimeType = do
  httpManager <- fmap _configHttpManager ask
  bots <- fmap _bots ask
  let (bot, _) = (M.!) bots MAIN 
  let fullUrl = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendDocument"

  let filePart = 
        (W.partBS "document" pdfBytes) 
        & W.partFileName ?~ (T.unpack filename)
        & W.partContentType ?~ TE.encodeUtf8 mimeType

  -- 1 Create the other parts for chat_id and caption.
  let chatPart = W.partText "chat_id" (tshow chatId)
  let captionPart = W.partText "caption" caption
  let parseModePart = W.partText "parse_mode" (tshow MarkdownV2)
  let opts = W.defaults & W.manager .~ Right httpManager
        -- The Content-Type header will be set automatically by wreq for multipart.
        -- We don't need to set it manually.
    
  -- 2. The 'post' function takes a list of 'Part's.
  let parts = [chatPart, captionPart, parseModePart, filePart]
  eResponse <- liftIO $ try' (W.postWith opts fullUrl parts)
  when (isLeft eResponse) $ $(logTM) ErrorS $ "ConsignmentNoteWatcher, telegram failure " <> ls (tshow eResponse)
