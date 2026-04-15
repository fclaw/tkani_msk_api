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

module Workers.FabricLifecycleObserver (runFabricLifecycleObserver) where


import Katip
import Data.Int (Int64)
import Data.Text (Text)
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
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG


import Text (encodeToText, tshow)
import App (AppM, _conciergeBotUrl, ChatKey (MAIN), _appDBPool, _bots)
import Concurrency (runJobWithCleanup)
import Domain.Warehouse.Enums (FabricLifecycle (..))
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Database (saveTemporaryNotificationMessage)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, MessageIdResponse (..))



data FabricLifecycleEvent = FabricLifecycleEvent
  { fabric_name   :: Text
  , new_lifecycle :: FabricLifecycle -- Your existing enum
  , hash          :: Int64
  } deriving (Show, Generic, FromJSON)

-- ==========================================================
--                  THE HELPER FUNCTIONS
-- ==========================================================

-- | Generates the call-to-action button text for a given lifecycle.
buttonText :: FabricLifecycle -> Text
buttonText lifecycle =
  case lifecycle of
    OnSale     -> "🔥 Забрать со скидкой"
    Clearance  -> "💥 Успеть купить остатки"
    NewArrival -> "🆕 Посмотреть новый лот"
    Advertised -> "💎 Перейти к лоту"
    _          -> "🧶 Подробнее в боте"

-- | Generates the notification text for a newly added fabric.
notificationText :: FabricLifecycle -> Text -> Text
notificationText lifecycle fabricName =
  let
    -- Choose the introductory phrase based on the lifecycle.
    introPhrase = case lifecycle of
      OnSale     -> "🔥 В распродажу добавлен новый товар"
      Clearance  -> "💥 В ликвидацию добавлен новый товар"
      NewArrival -> "✨ В новинки добавлен"
      Advertised -> "🔔 Анонсирован новый товар"
      _          -> "✅ В каталог добавлен новый товар" -- Fallback for Regular, etc.

  in introPhrase <> ": *" <> fabricName <> "*!"


runFabricLifecycleObserver :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runFabricLifecycleObserver connInfo appMToHandler = do
  $(logTM) InfoS "Fabric Lifecycle Observer started."
  liftIO $ PG.withConnect connInfo $ \conn -> do
    -- 1. Subscribe to the channel. This must be done on the connection.
    void $ PG.execute_ conn "LISTEN fabric_lifecycle_events"
    -- 2. Enter an infinite loop to wait for notifications.
    forever $ do
      -- 'getNotification' blocks until a notification is received.
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      putStrLn $ "Received notification: " <> show payload
      let ePayload = eitherDecode @FabricLifecycleEvent $ BL.fromStrict payload
      void $ async $
        -- We still run the main logic inside 'appMToHandler' to get the AppM context,
        -- but now it's happening in the background.
        void $ appMToHandler $ runJobWithCleanup (processSingleJob ePayload)

processSingleJob :: Either String FabricLifecycleEvent -> AppM ()
processSingleJob (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (FabricLifecycleEvent), error: " <> err
processSingleJob (Right event) = do
  $(logTM) InfoS $ ls $ "Processing FabricLifecycleEvent: " <> show event
  -- Here you would add your logic to handle the lifecycle change,
  -- e.g., updating the database, notifying other services, etc.
  $(logTM) InfoS $ "Received lifecycle change event for fabric: " <> ls (fabric_name event)
  let targetLifecycle = new_lifecycle event
  cfg <- ask

  let botUrl = _conciergeBotUrl cfg
  let deepLinkUrl = botUrl <> "?start=" <> encodeToText targetLifecycle <> "_" <> tshow (hash event)
  let keyboard = 
       object
       [ "inline_keyboard" .=
        [[ object 
          [ "text" .= buttonText targetLifecycle
          , "url"  .= deepLinkUrl
          ]
        ]]
       ]
  let adsMsg = escapeMarkdownV2 $ notificationText targetLifecycle (fabric_name event)
  -- 4. Send the new, small notification message to the MAIN channel.
  eTelResp <- sendOrEditTelegramMessage mempty adsMsg MAIN Nothing Nothing (Just keyboard)
  for_ eTelResp $ \MessageIdResponse {..} -> do
    $(logTM) InfoS $ "Sent fabric lifecycle notification for " <> ls (fabric_name event)
    pool <- fmap _appDBPool ask
    bots <- fmap _bots ask
    let botsInfo = M.lookup MAIN bots
    for_ botsInfo $ \(_, chatId) ->
      saveTemporaryNotificationMessage chatId message_id pool
