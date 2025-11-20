-- file: src/Infrastructure/Telegram.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Infrastructure.Services.Telegram
  ( sendOrEditTelegramMessage
  , deleteMessage
  , TelegramError(..)
  , MessageIdResponse (..)
  )
where


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
import           Data.Aeson.TH
import           Data.Maybe (fromMaybe)
import           Data.Int (Int64)
import qualified Data.Map.Strict         as M
import           Data.Traversable        (for)   

-- (Assuming your AppM and Config are defined in App)
import           App (Config(..), AppM, ChatKey)
import           Text (recordLabelModifier)


-- Custom Error Type for better error handling
data TelegramError
  = ApiRequestFailed SomeException
  | JSONError T.Text
  | TelegramApiError Text           -- ^ Telegram returned ok:false with an error description
  | BotNotFound
  deriving (Show)


-- You'll need to define a FromJSON instance for this to parse the message_id
newtype MessageIdResponse = MessageIdResponse { message_id :: Int }
  deriving (Show, Generic)
instance A.FromJSON MessageIdResponse where
  parseJSON = A.withObject "Message" $ \o -> fmap MessageIdResponse (o A..: "result" >>= (A..: "message_id"))

-- A simple wrapper around 'try' for better type inference if needed.
try' :: IO a -> IO (Either SomeException a)
try' = try

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
  -> ChatKey                      -- ^ The target chat
  -> Maybe Int                    -- ^ The target message_id
  -> AppM (Either TelegramError MessageIdResponse)
sendOrEditTelegramMessage context messageText chatKey mMessageId = do
  -- 1. Get the necessary config from our application environment
  bots <- fmap _bots ask
  let botsInfo = M.lookup chatKey bots
  res <- for botsInfo $ \(bot, chat) -> do
    httpManager <- fmap _configHttpManager ask -- Assumes Manager is in your Config
    let (endpoint, messageIdField) =
          case mMessageId of
            -- If we have no messageId, we use the 'sendMessage' endpoint
            Nothing -> ("sendMessage", A.fromList [])
            -- If we have a messageId, we use 'editMessageText' and add the field
            Just msgId -> ("editMessageText", A.fromList ["message_id" A..= msgId])

    -- 2. Construct the API URL
    let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/" <> endpoint

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
    
  case res of Nothing -> pure $ Left BotNotFound; Just res -> pure $ res;


-- | Represents a generic response from the Telegram API.
--   We parameterize it by 'a' which will be the type of the 'result' field.
data TelegramResponse a = TelegramResponse
  { trOk          :: Bool
  , trDescription :: Maybe Text
  , trResult      :: Maybe a
  } deriving (Show, Generic)

-- | Automatically derive a FromJSON instance to parse the response.
$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "tr" } ''TelegramResponse)


deleteMessage :: Int -> ChatKey -> AppM (Either TelegramError ())
deleteMessage messageId chatKey = do
  -- 1. Get the necessary config from our application environment
  bots <- fmap _bots ask
  let botsInfo = M.lookup chatKey bots
  res <- for botsInfo $ \(bot, chat) -> do
    httpManager <- fmap _configHttpManager ask
    -- 2. Construct the API URL for the 'deleteMessage' endpoint
    let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/deleteMessage"

    -- 3. The JSON payload required by the endpoint
    let payload = A.Object $ A.fromList
          [ "chat_id"    A..= chat
          , "message_id" A..= messageId
          ]

    -- 4. Perform the API call using the shared HTTP manager, wrapped in an exception handler
    eResult <- liftIO $ try' $ postWith (defaults & manager .~ Right httpManager) url payload

    -- 5. Handle the result, distinguishing between network, parsing, and API errors
    case eResult of
      -- The network request itself failed (e.g., timeout)
      Left err -> do
        $(logTM) ErrorS $ "CRITICAL: Failed to delete Telegram message " <> ls (show messageId) <> ". Error: " <> ls (show err)
        pure $ Left (ApiRequestFailed err)

      -- The network request succeeded, now we inspect the response
      Right response -> do
        -- Attempt to parse the response body. For 'deleteMessage', the 'result' is just a boolean.
        let apiResponse = A.eitherDecode @(TelegramResponse Bool) (response ^. responseBody)
        
        pure $ case apiResponse of
          -- The response was not valid JSON or didn't match our data type
          Left parseError ->
            Left $ JSONError (T.pack parseError)
          
          -- We successfully parsed the response, now check the 'ok' field
          Right tgResp ->
            if trOk tgResp
              then Right () -- Success!
              else Left $ TelegramApiError (fromMaybe "Unknown API error" (trDescription tgResp))

  case res of Nothing -> pure $ Left BotNotFound; Just res -> pure $ res;    