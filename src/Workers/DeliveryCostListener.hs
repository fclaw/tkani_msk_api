{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}

module Workers.DeliveryCostListener (runDeliveryCostListener) where


import Katip
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void)
import Data.Int (Int64)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Data.Foldable (for_)
import Servant.Server (ServerError)
import Data.Aeson (FromJSON, eitherDecode)
import Control.Concurrent.Async (async)
import Control.Monad.Reader.Class (ask)
import qualified Data.ByteString.Lazy as BL
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.Map.Strict         as M
import qualified Data.Aeson              as A
import Network.Wreq
import Control.Lens            ((&), (.~), (^.))
import qualified Database.PostgreSQL.Simple.Notification as PG

import Text (tshow)
import App (AppM, ChatKey (MAIN), _bots, _configHttpManager)
import Concurrency (runJobWithCleanup)
import Infrastructure.Services.Telegram (ParseMode (MarkdownV2))
import Utils.Telegram.Markdown (escapeMarkdownV2)


data DeliveryCost =
     DeliveryCost 
     { delivery_cost :: Int
     , chat_id       :: Int64
     , message_id    :: Int64
     , order_id      :: Text
     } deriving (Show, Generic, FromJSON)


runDeliveryCostListener :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runDeliveryCostListener connInfo appMToHandler = do
  $(logTM) InfoS "SDEK Generate Receipt Listener started."
  liftIO $ PG.withConnect connInfo $ \conn -> do
    -- 1. Subscribe to the channel. This must be done on the connection.
    void $ PG.execute_ conn "LISTEN delivery_cost_jobs"
    -- 2. Loop forever, waiting for notifications.
    forever $ do
      -- PG.getNotification blocks until a message arrives.
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      putStrLn $ "Received notification: " <> show payload
      let ePayload = eitherDecode @DeliveryCost $ BL.fromStrict payload
      void $ async $
        -- We still run the main logic inside 'appMToHandler' to get the AppM context,
        -- but now it's happening in the background.
        void $ appMToHandler $ runJobWithCleanup (processSingleJob ePayload)

processSingleJob :: Either String DeliveryCost -> AppM ()
processSingleJob (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (DeliveryCost), error: " <> err
processSingleJob (Right DeliveryCost {..}) = do
  let message = 
       escapeMarkdownV2 $ 
         "**Номер заказа:** " <> order_id <> 
         "\n" <> "**Стоимость доставки:** " <> 
         tshow delivery_cost <> " руб."
  bots <- fmap _bots ask
  let botsInfo = M.lookup MAIN bots
  for_ botsInfo $ \(bot, _) -> do
    httpManager <- fmap _configHttpManager ask -- Assumes Manager is in your Config
    let url = "https://api.telegram.org/bot" <> unpack bot <> "/sendMessage"
    let payload = A.object
          [ "chat_id"             A..= chat_id
          , "reply_to_message_id" A..= message_id
          , "text"                A..= message
          , "parse_mode"          A..= tshow MarkdownV2
          ]
    void $ liftIO $ postWith (defaults & manager .~ Right httpManager) url payload