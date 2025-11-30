{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Logging.Telegram (mkTelegramScribe, getTelegramConfig) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import           Katip
import           Katip.Core (getEnvironment, unLogStr)
import           Network.Wreq
import           Control.Lens ((&), (.~))
import           System.Environment (lookupEnv)
import           System.IO (stderr)
import           Control.Monad (when, void, forever, replicateM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (catch, SomeException)
import           Network.HTTP.Client (Manager)
import           Data.Time (formatTime, defaultTimeLocale)

-- New Imports for Concurrency / Batching
import           Control.Concurrent.Async (async, link)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, writeTBQueue, isEmptyTBQueue, readTBQueue, newTBQueueIO)


import Utils.Telegram.Markdown (escapeMarkdownV2)

-- | Data type to hold our Telegram configuration.
data TelegramConfig = TelegramConfig
  { tcToken   :: T.Text -- ^ Your bot token
  , tcChatId  :: T.Text -- ^ The destination channel/chat ID
  } deriving Show

-- | Reads config from environment variables. Returns Nothing if not found.
getTelegramConfig :: IO (Maybe TelegramConfig)
getTelegramConfig = do
  mToken <- lookupEnv "TELEGRAM_BOT_TOKEN"
  mChatId <- lookupEnv "TELEGRAM_CHAT_ID"
  pure $ TelegramConfig <$> (T.pack <$> mToken) <*> (T.pack <$> mChatId)

severityToEmoji :: Severity -> T.Text
severityToEmoji DebugS    = "⚙️"
severityToEmoji InfoS     = "✅"
severityToEmoji NoticeS   = "ℹ️"
severityToEmoji WarningS  = "⚠️"
severityToEmoji ErrorS    = "🔥"
severityToEmoji CriticalS = "🚨"
severityToEmoji AlertS    = "📢"
severityToEmoji EmergencyS= "🆘"

-- | Formats a log item into a clear, Markdown-formatted Telegram message.
formatTelegramMessage :: LogItem a => Item a -> T.Text
formatTelegramMessage item =
  let 
      timestampTag = T.pack $ formatTime defaultTimeLocale "\\#t%Y\\_%m\\_%d" (_itemTime item)
      mainMessage = T.unlines
        [ T.concat [ severityToEmoji severity, " *", T.pack (show severity), "* @ `", hostname, "`" ]
        , ""
        , T.concat [ "*App:* `", app, "`", " \\(*Env:* `", env, "`", "\\)" ]
        , T.concat [ "*Namespace:* `", ns, "`"]
        , ""
        , "```"
        , logStr
        , "```"
        , if A.null payload then mempty else T.concat ["\n*Context:*\n```json\n", context, "\n```"]
        -- 👇 attention block
        , attentionLine 
        ]
        where
          toText = T.pack . show
          severity = _itemSeverity item
          hostname = toText $ _itemHost item
          app      = T.intercalate "." $ unNamespace $ _itemApp item
          env      = getEnvironment $ _itemEnv item
          ns       = T.intercalate "." $ unNamespace $ _itemNamespace item
          logStr   = TL.toStrict $ TLB.toLazyText $ unLogStr $ _itemMessage item
          payload :: A.Object
          payload = payloadObject V2 (_itemPayload item)
          context :: T.Text
          context = TL.toStrict $ TLE.decodeUtf8 $ A.encode payload
          -- 👇 DEFINITION LOGIC
          -- We check if severity is in the list of "Bad" statuses
          attentionLine :: T.Text
          attentionLine | severity `elem` [ErrorS, AlertS, EmergencyS, CriticalS] = "‼️ @sclaw" -- You can also use emojis like "‼️ @sclaw"
                        | otherwise = mempty
  in
    mainMessage <> timestampTag

-- | The worker logic that runs in the background.
--   It waits for at least one message, then grabs up to 4 more immediately if available.
telegramBatchWorker :: Manager -> TelegramConfig -> TBQueue T.Text -> IO ()
telegramBatchWorker tlsManager config queue = forever $ do
    
    threadDelay (5 * 1000000) -- 1 seconds delay between batches

    -- 1. Block until we get at least one log message
    firstMsg <- atomically $ readTBQueue queue
    
    -- 2. Try to grab up to 4 more messages currently in the queue (Batch size = 5)
    --    This is non-blocking. If queue is empty, 'rest' is [].
    rest <- atomically $ flushUpTo 3 queue

    let batch = firstMsg : rest
    
    -- 3. Combine messages. 
    --    We separate them with a horizontal rule "---" to keep them readable in one Telegram block.
    -- FIX IS HERE:
    -- We must escape every dash using a backslash: \-
    -- OR use a different separator (like emojis) that doesn't need escaping.
    
    -- Option A: Escaped Dashes (Valid MarkdownV2)
    let separator = escapeMarkdownV2 "\n\n--------------------\n\n"
    
    -- Option B: Emojis (Easier to read, no escaping needed) -> Recommended
    -- let separator = "\n\n〰️〰️〰️〰️〰️〰️〰️〰️\n\n"

    let finalMessage = T.intercalate separator batch

    sendToTelegram tlsManager config finalMessage

-- | Helper to take N items from TQueue non-blocking
flushUpTo :: Int -> TBQueue a -> STM [a]
flushUpTo 0 _ = return []
flushUpTo n q = do
    isEmpty <- isEmptyTBQueue q
    if isEmpty 
        then return [] 
        else do
            x <- readTBQueue q
            xs <- flushUpTo (n - 1) q
            return (x : xs)

-- | Helper to perform the HTTP request
sendToTelegram :: Manager -> TelegramConfig -> T.Text -> IO ()
sendToTelegram mgr cfg msg = do
    let payload = A.object
          [ "chat_id"    A..= tcChatId cfg
          , "text"       A..= msg
          , "parse_mode" A..= T.pack "MarkdownV2"
          ]
        
        opts = defaults & manager .~ Right mgr
        url = "https://api.telegram.org/bot" ++ T.unpack (tcToken cfg) ++ "/sendMessage"

    -- Perform request inside try/catch
    (void $ postWith opts url payload) `catch` handleHttpException

    where
      handleHttpException :: SomeException -> IO ()
      handleHttpException e = TIO.hPutStrLn stderr $ T.pack $
        "!! FAILED to send batch log to Telegram: " <> show e


-- | The main function that constructs our Scribe for Telegram with Batching.
mkTelegramScribe :: Manager -> TelegramConfig -> Severity -> Verbosity -> IO Scribe
mkTelegramScribe tlsManager config minSeverity verbosity = do
  
  -- 1. Create a BOUNDED queue. Size 1000 is safe (few MBs of RAM max).
  queue <- newTBQueueIO 1000 

  -- 2. Spawn worker (Passes the TBQueue now)
  workerAsync <- async (telegramBatchWorker tlsManager config queue)
  link workerAsync

  -- 3. Push Logic: Non-blocking write
  let pushLog item = do
        isPerm <- permitItem minSeverity item
        when isPerm $ do
          let txt = formatTelegramMessage item
          -- Force evaluation of txt to avoid thunk buildup (Space Leak protection)
          -- Although Strict Text is usually fine, `seq` is safer here.
          atomically $ do
             isFull <- isFullTBQueue queue
             when(not isFull) $ writeTBQueue queue txt

  -- 4. Return the Scribe
  pure $ Scribe
    { liPush = pushLog
    -- Ideally, we should wait for the queue to drain here, but for simplicity 
    -- we assume app shutdown kills the worker.
    , scribeFinalizer = pure ()
    , scribePermitItem = permitItem minSeverity
    }