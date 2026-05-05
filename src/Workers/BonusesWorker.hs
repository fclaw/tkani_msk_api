{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Workers.BonusesWorker (runBonusesWorker) where

import Katip
import Data.Int (Int64)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Data.Foldable (for_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void)
import Control.Concurrent.Async (async)
import Servant.Server (ServerError)
import qualified Data.Map.Strict as M
import Control.Monad.Reader.Class (ask)
import Data.Aeson (FromJSON, eitherDecode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Traversable (for)
import qualified Data.Aeson as A
import Network.Wreq hiding (get)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson.KeyMap as A
import qualified Data.HashMap.Strict as HM
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG


import Text (tshow)
import TH.Location (currentModule)
import Concurrency (runJobWithCleanup)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (ParseMode (MarkdownV2))
import App (AppM, _appDBPool, _bots, _configHttpManager, ChatKey (MAIN), render)
import Infrastructure.Database (fetchBonusesDetails, adjustBonuses, BonusesDetails (..))


data BonusesAddedEvent =
     BonusesAddedEvent
    { payment_id   :: Int64
    } deriving (Show, Generic, FromJSON)


runBonusesWorker :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runBonusesWorker connInfo appMToHandler = do
  $(logTM) InfoS "Bonuses worker started."
  liftIO $ PG.withConnect connInfo $ \conn -> do
    -- 1. Subscribe to the channel. This must be done on the connection.
    void $ PG.execute_ conn "LISTEN bonuses_added_events"
    -- 2. Enter an infinite loop to wait for notifications.
    forever $ do
      -- 'getNotification' blocks until a notification is received.
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      putStrLn $ "Received notification: " <> show payload
      let ePayload = eitherDecode @BonusesAddedEvent $ BL.fromStrict payload
      void $ async $
        -- We still run the main logic inside 'appMToHandler' to get the AppM context,
        -- but now it's happening in the background.
        void $ appMToHandler $ runJobWithCleanup (processSingleJob ePayload)

processSingleJob :: Either String BonusesAddedEvent -> AppM ()
processSingleJob (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (BonusesAddedEvent), error: " <> err
processSingleJob (Right BonusesAddedEvent {..}) = do
  $(logTM) InfoS $ ls $ "Processing BonusesAddedEvent for payment_id: " <> show payment_id
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- fetchBonusesDetails payment_id pool
  case eDbRes of
    Left dbErr -> 
      $(logTM) ErrorS $ ls $ 
        "Database error while fetching \
        \ bonus details for payment_id " <> 
        show payment_id <> ": " <> show dbErr
    Right bonusDetails@BonusesDetails {..} -> do
      -- Log the bonus details (or you could do more complex processing here)
      $(logTM) InfoS $ ls $ "Fetched bonus details for payment_id " <> show payment_id <> ": " <> show bonusDetails
      eDbRed <- adjustBonuses bdUserId bdEarnedBonuses bdExpendedBonuses pool
      case eDbRed of
        Left dbErr -> 
          $(logTM) ErrorS $ ls $ 
            "Database error while adjusting \
            \ bonuses for payment_id " <> 
            show payment_id <> ": " <> show dbErr
        Right _ -> do
          bots <- fmap _bots ask
          let botsInfo = M.lookup MAIN bots
          for_ botsInfo $ \(bot, _) -> do
            httpManager <- fmap _configHttpManager ask -- Assumes Manager is in your Config
            let templateData = 
                  HM.fromList 
                   [("earnedBonuses", tshow bdEarnedBonuses), 
                    ("expendedBonuses", tshow bdExpendedBonuses), 
                    ("currentBalance", tshow (bdCurrentBonuses + 
                                              bdEarnedBonuses - 
                                              bdExpendedBonuses))]
            msg <- render $currentModule templateData
            let url = "https://api.telegram.org/bot" <> unpack bot <> "/sendMessage"
            let payload = 
                  A.Object $ A.fromList
                   [ ("chat_id"    A..= pack (show bdChatId))
                   , ("text"       A..= escapeMarkdownV2 msg)
                   , ("parse_mode" A..= tshow MarkdownV2)
                   ]
            void $ liftIO $ postWith (defaults & manager .~ Right httpManager) url payload 