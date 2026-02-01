{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Workers.ShelfSubmissionObserver (runShelfSubmissionObserver) where


import Katip
import Data.Aeson
import Data.Aeson.TH
import Data.Int (Int64)
import GHC.Generics (Generic)
import Data.Foldable (for_)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (when)
import Data.Either (isLeft)
import Data.Aeson.KeyMap as A
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void, when)
import Servant.Server (ServerError)
import Control.Monad.Reader.Class (ask)
import qualified Data.Map.Strict as M
import Network.Wreq hiding (JSONError)
import Control.Lens ((&), (.~), (^.), (?~))
import Control.Concurrent.Async (async)
import qualified Data.ByteString.Lazy as BL
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG



import App (AppM, _bots, ChatKey (MAIN), _configHttpManager)
import API.Types (ShelfStatus (..))
import Concurrency (runJobWithCleanup)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (ParseMode (..), MessageIdResponse)



data SubmissionEvent =
     SubmissionEvent
     { shelf_id            :: Int64
     , telegram_user_id    :: Int64
     , new_status          :: ShelfStatus
     , reply_to_chat_id    :: Int64
     , reply_to_message_id :: Int64
     } deriving (Show, Generic, FromJSON)


runShelfSubmissionObserver :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runShelfSubmissionObserver connInfo appMToHandler = do 
  $(logTM) InfoS "Shelf submission Listener started."
  liftIO $ PG.withConnect connInfo $ \conn -> do
    -- 1. Subscribe to the channel. This must be done on the connection.
    void $ PG.execute_ conn "LISTEN shelf_status_events"
    -- 2. Loop forever, waiting for notifications.
    forever $ do
      -- PG.getNotification blocks until a message arrives.
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      putStrLn $ "Received notification: " <> show payload
      let ePayload = eitherDecode @SubmissionEvent $ BL.fromStrict payload
      -- === THIS IS THE FIX ===
      -- Fork a new, lightweight thread to do the heavy lifting.
      -- The 'forever' loop can immediately continue to the next 'getNotification'.
      void $ async $
        -- We still run the main logic inside 'appMToHandler' to get the AppM context,
        -- but now it's happening in the background.
        void $ appMToHandler $ runJobWithCleanup (processSingleEvent ePayload)

processSingleEvent :: Either String SubmissionEvent -> AppM ()
processSingleEvent (Left err) = $(logTM) ErrorS $ ls $ "Failed to parse payload (SubmissionEvent), error: " <> err
processSingleEvent (Right SubmissionEvent {..}) = replyToTelegramMessage new_status reply_to_chat_id reply_to_message_id



replyToTelegramMessage :: ShelfStatus -> Int64 -> Int64 -> AppM ()
replyToTelegramMessage status replyToChatId replyToMessageId = do
  bots <- fmap _bots ask
  let botsInfo = M.lookup MAIN bots
  for_ botsInfo $ \(bot, _) -> do
    httpManager <- fmap _configHttpManager ask
    let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendMessage"
    -- Determine the message content and reply markup based on the new status.
    -- This encapsulates all the presentation logic.
    let messageText = 
          case status of
            -- Case: The user's application was APPROVED.
            Active ->
                 T.unlines
                  [ "🎉 **Ваша «Виртуальная полка» готова!**"
                  , ""
                  , "Поздравляем! Ваша заявка одобрена, и ваш личный склад для покупок активирован."
                  , ""
                  , "Для доступа к вашей полке в любой момент используйте команду /shelf."
                  ]
            
            -- Case: The user was WAITLISTED.
            Waitlisted ->
                 T.unlines
                  [ "👋 **Спасибо за ваш интерес к «Виртуальной полке»!**"
                  , ""
                  , "На данный момент мы активируем эту функцию для пользователей поэтапно, и ваш регион пока не подключен."
                  , ""
                  , "Мы добавили вас в список ожидания и сообщим, как только функция станет доступна. Вам не нужно ничего делать."
                  ]
    let payload =
          Object $ A.fromList
          [ "chat_id"              .= replyToChatId
          , "text"                 .= escapeMarkdownV2 messageText
          , "parse_mode"           .= T.pack (show MarkdownV2)
          , "reply_to_message_id"  .= replyToMessageId
          ]
    eTelResp <- liftIO $ try @SomeException $ postWith (defaults & manager .~ Right httpManager) url payload
    when (isLeft eTelResp) $ $(logTM) ErrorS $ "telegram failed to deliver message " <> ls (show eTelResp)

    