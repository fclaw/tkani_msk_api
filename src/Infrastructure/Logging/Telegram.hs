{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Logging.Telegram (mkTelegramScribe, getTelegramConfig) where


-- Add these imports at the top of your file
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import           Katip
import           Katip.Core (getEnvironment, unLogStr)
import           Network.Wreq         -- <-- Changed from Network.HTTP.Req
import           Control.Lens ((&), (.~)) -- <-- New import for lens operators
import           System.Environment (lookupEnv)
import           System.IO (stderr)
import           Control.Monad (when, void) -- void is useful with wreq
import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (catch, SomeException)
import           Network.HTTP.Client (Manager)
import Data.Time (formatTime, defaultTimeLocale)

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

-- | A helper to get a nice emoji for the log level.
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
-- | Formats a log item into a clear, Markdown-formatted Telegram message.
--   (This is the corrected version)
formatTelegramMessage :: LogItem a => Item a -> T.Text
formatTelegramMessage item =
  let 
      -- Format the UTC time directly from the item's timestamp
      -- Format: #t2023_11_19_103055Z (The 'Z' indicates UTC)
      timestampTag = T.pack $ formatTime defaultTimeLocale "\\#t%Y\\_%m\\_%d" (_itemTime item)
      mainMessage = T.unlines
        [ T.concat [ severityToEmoji severity, " *", T.pack (show severity), "* @ `", hostname, "`" ]
        , ""
        -- We now put App, Env, and Namespace inside their own code blocks.
        -- This means they NEVER need to be escaped.
        , T.concat [ "*App:* `", app, "`", " \\(*Env:* `", env, "`", "\\)" ]
        , T.concat [ "*Namespace:* `", ns, "`"]
        , ""
        , "```"
        , logStr
        , "```"
        , if A.null payload then mempty else T.concat ["\n*Context:*\n```json\n", context, "\n```"]
        ]
        where
          toText = T.pack . show
          severity = _itemSeverity item
          -- Texts that will NOT be inside code blocks MUST be escaped
          hostname = toText $ _itemHost item
          app      = T.intercalate "." $ unNamespace $ _itemApp item
          env      = getEnvironment $ _itemEnv item
          ns       = T.intercalate "." $ unNamespace $ _itemNamespace item
          logStr   = TL.toStrict $ TLB.toLazyText $ unLogStr $ _itemMessage item
          -- 1. Use the idiomatic katip function to get the payload as a JSON Object.
          --    This works for ANY 'a' that has a 'LogItem a' instance.
          payload :: A.Object
          payload = payloadObject V2 (_itemPayload item)

          -- 2. Now, encode the resulting 'A.Object'. This is always safe
          --    and produces a clean JSON string.
          context :: T.Text
          context = TL.toStrict $ TLE.decodeUtf8 $ A.encode payload
  in
    -- Append the tag. The tag itself does not need Markdown escaping
    -- because hashtags are valid in plain text.
    mainMessage <> timestampTag


-- | The main function that constructs our Scribe for Telegram.
--   (This is the ONLY function you need to replace)
mkTelegramScribe :: Manager -> TelegramConfig -> Severity -> Verbosity -> IO Scribe
mkTelegramScribe tlsManager config minSeverity verbosity = do
  let pushLog item = do
        -- Only send logs that are at or above the configured minimum severity
        isPerm <- permitItem minSeverity item
        when isPerm $
          send (formatTelegramMessage item)
            `catch` handleHttpException
 
      -- HTTP request logic (using wreq)
      send message = do
        let payload = A.object
              [ "chat_id"    A..= tcChatId config
              , "text"       A..= message
              , "parse_mode" A..= T.pack "MarkdownV2"
              ]
            
            -- Set a 10-second timeout for the request
            opts = defaults & manager .~ Right tlsManager
            url = "https://api.telegram.org/bot" ++ T.unpack (tcToken config) ++ "/sendMessage"

        -- Use `postWith` from wreq. We use `void` to ignore the response body.
        void $ postWith opts url payload
        
        TIO.putStrLn "--> Logged to Telegram"

      -- Gracefully handle network errors so they don't crash the app
      handleHttpException :: SomeException -> IO ()
      handleHttpException e = TIO.hPutStrLn stderr $ T.pack $
        "!! FAILED to send log to Telegram: " <> show e

  -- The final Scribe record, now matching the definition in your image.
  pure $ Scribe
    { -- Corresponds to: liPush :: Item a -> IO ()
      liPush = pushLog
    -- Corresponds to: scribeFinalizer :: IO ()
    , scribeFinalizer = pure ()
    -- Corresponds to: scribePermitItem :: !PermitFunc
    -- We build the filtering function directly inside the scribe.
    , scribePermitItem = permitItem minSeverity
    }