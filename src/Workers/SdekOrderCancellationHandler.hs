{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Workers.SdekOrderCancellationHandler (runSdekOrderCancellationHandler) where

import Katip
import Data.Maybe (isJust)
import Data.Int (Int64)
import Data.UUID (UUID)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant.Server (ServerError)
import Data.Aeson (FromJSON, eitherDecode)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async (async)
import qualified Data.ByteString.Lazy as BL
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG

import Text (tshow)
import Concurrency (runJobWithCleanup)
import TH.Location (currentModule)
import Text (tshow, encodeToText)
import App (AppM, ChatKey (ORDER), extractFromEither)
import Infrastructure.Services.Sdek (cancelOrder)
import Infrastructure.Services.Sdek.Types (corErrors)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)


-- The payload from your pg_notify trigger
data CancellationEventPayload = 
     CancellationEventPayload
     { order_id  :: Text
     , sdek_uuid :: UUID -- Or Text, depending on your type
     } deriving (Show, Generic, FromJSON)


runSdekOrderCancellationHandler :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runSdekOrderCancellationHandler connInfo runAppM = do
  $(logTM) InfoS "SDEK Cancellation Listener started."
  liftIO $ PG.withConnect connInfo $ \conn -> do
    -- 1. Subscribe to the channel. This must be done on the connection.
    void $ PG.execute_ conn "LISTEN order_cancel_events"
    -- 2. Loop forever, waiting for notifications.
    forever $ do
      -- PG.getNotification blocks until a message arrives.
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      putStrLn $ "Received notification: " <> show payload
      let ePayload = eitherDecode @CancellationEventPayload $ BL.fromStrict payload
      void $ async $
        -- We still run the main logic inside 'runAppM' to get the AppM context,
        -- but now it's happening in the background.
        void $ runAppM $ runJobWithCleanup (processSingleJob ePayload)

processSingleJob :: Either String CancellationEventPayload -> AppM ()
processSingleJob (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (ReceiptJob), error: " <> err
processSingleJob (Right CancellationEventPayload {..}) = do 
  $(logTM) InfoS $ ls $ "Received cancellation job for order: " <> order_id
  eSdekResp <- cancelOrder sdek_uuid
  extractFromEither eSdekResp $ \resp -> do
    if isJust (corErrors resp) then do
      let errMsg = 
           escapeMarkdownV2 $ 
             "order " <> order_id <> 
             " cannot be deleted from SDEK.\
             \ Manual intervention is required. errors: " <>
             tshow (corErrors resp)
      void $ sendOrEditTelegramMessage mempty errMsg ORDER Nothing Nothing Nothing
    else $(logTM) InfoS $ "Successfully sent cancellation request to SDEK for order UUID: " <> ls (tshow sdek_uuid)