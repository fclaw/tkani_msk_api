-- file: src/Infrastructure/Telegram.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Infrastructure.Services.Telegram
  ( sendOrEditTelegramMessage
  , TelegramError(..)
  , MessageIdResponse (..)
  )
where

-- (Assuming your AppM and Config are defined in App)
import           App (Config(..), AppM)

-- Standard & Third-Party Imports
import           Control.Exception       (SomeException, try)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad           (void)
import           Control.Monad.Reader.Class (ask)
import qualified Data.Aeson              as A
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Network.Wreq            hiding (JSONError)
import           Control.Lens            ((&), (.~), (^.))
import           Katip
import qualified Data.ByteString.Lazy as LBS
import           GHC.Generics
import           Data.Aeson.KeyMap       as A


-- Custom Error Type for better error handling
data TelegramError
  = ApiRequestFailed SomeException
  | JSONError T.Text
  deriving (Show)


-- You'll need to define a FromJSON instance for this to parse the message_id
newtype MessageIdResponse = MessageIdResponse { message_id :: Int }
  deriving (Show, Generic)
instance A.FromJSON MessageIdResponse where
  parseJSON = A.withObject "Message" $ \o -> fmap MessageIdResponse (o A..: "result" >>= (A..: "message_id"))

-- | Sends a text message to a specified Telegram chat (channel or user).
--   This function is designed to be called from within your AppM monad.
--
--   Usage:
--   > eResult <- sendOrEditTelegramMessage myChatId "Hello, *World*\\!" Nothing
--   > case eResult of
--   >   Left err -> $(logTM) ErrorS ...
--   >   Right _  -> $(logTM) InfoS ...
sendOrEditTelegramMessage
  :: Text                         -- ^ The message text, pre-formatted with MarkdownV2
  -> Text
  -> Maybe Int                    -- ^ The target chat_id
  -> AppM (Either TelegramError MessageIdResponse)
sendOrEditTelegramMessage context messageText mMessageId = do
  -- 1. Get the necessary config from our application environment
  botToken <- fmap _configBotToken ask
  chat <- fmap _orderChatId ask
  httpManager <- fmap _configHttpManager ask -- Assumes Manager is in your Config

  let (endpoint, messageIdField) =
        case mMessageId of
          -- If we have no messageId, we use the 'sendMessage' endpoint
          Nothing -> ("sendMessage", A.fromList [])
          -- If we have a messageId, we use 'editMessageText' and add the field
          Just msgId -> ("editMessageText", A.fromList ["message_id" A..= msgId])

  -- 2. Construct the API URL
  let url = "https://api.telegram.org/bot" <> T.unpack botToken <> "/" <> endpoint

  -- 3. The JSON payload for the sendMessage endpoint
  let basePayload = A.fromList
       [ "chat_id"    A..= chat
       , "text"       A..= messageText
       , "parse_mode" A..= T.pack "MarkdownV2"
       ]

        -- Combine the base payload with the conditional message_id field
  let payload = A.Object $ basePayload `A.union` messageIdField

  -- 4. Perform the API call using the shared HTTP manager.
  --    'liftIO' is used to run the IO action inside our AppM stack.
  --    'try' will catch any network exceptions.
  eResult <- liftIO $ try' $  postWith (defaults & manager .~ Right httpManager) url payload

  -- 5. Wrap the result in our custom error type for clean handling.
  case eResult of
    Right response -> do 
      let mRes = A.eitherDecode @MessageIdResponse (response ^. responseBody)
      return $ either (Left . JSONError . T.pack . show) Right mRes
    Left err  -> do 
      $(logTM) ErrorS $ "CRITICAL: Failed to send a notification for " <> ls context <> ". Error: " <> ls (show err)
      pure $ Left (ApiRequestFailed err)

-- A simple wrapper around 'try' for better type inference if needed.
try' :: IO a -> IO (Either SomeException a)
try' = try