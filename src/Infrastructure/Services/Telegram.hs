-- file: src/Infrastructure/Telegram.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Infrastructure.Services.Telegram
  ( sendOrEditTelegramMessage
  , deleteMessage
  , sendPhotoToTelegram
  , editMessageMediaWithPhoto
  , sendDocument
  , forwardTelegramMessage
  , TelegramError(..)
  , MessageIdResponse (..)
  , ParseMode (..)
  , disableLinkPreviewOption
  , try'
  )
where


-- Standard & Third-Party Imports
import           Control.Exception       (SomeException, try, toException, Exception)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad           (void, when)
import           Control.Monad.Reader.Class (ask)
import qualified Data.Aeson              as A
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Network.Wreq            hiding (JSONError)
import           Control.Lens            ((&), (.~), (^.), (?~))
import           Katip
import qualified Data.ByteString.Lazy as LBS
import           GHC.Generics
import           Data.Aeson.KeyMap       as A
import           Data.Aeson.TH
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Int (Int64)
import qualified Data.Map.Strict         as M
import           Data.Traversable        (for)
import qualified Data.Text.Encoding      as TE
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString         as B
import           Data.Bifunctor (first)
import           Data.Either (isLeft)
import qualified Network.Wreq as W


-- (Assuming your AppM and Config are defined in App)
import           App (Config(..), AppM, ChatKey)
import           Text (recordLabelModifier, tshow)


-- Custom Error Type for better error handling
data TelegramError
  = ApiRequestFailed SomeException
  | JSONError T.Text
  | TelegramApiError Text           -- ^ Telegram returned ok:false with an error description
  | BotNotFound
  deriving (Show)

deriving instance Exception TelegramError

data ParseMode = MarkdownV2 | Markdown 
  deriving Show


data LinkPreviewOptions = 
     LinkPreviewOptions
     { is_disabled        :: Bool    -- is_disabled
     , url                :: Text    -- url
     , prefer_small_media :: Bool    -- prefer_small_media
     , prefer_large_media :: Bool    -- prefer_large_media
     , show_above_text    :: Bool    -- show_above_text
     } deriving (Show, Eq, Generic, A.ToJSON)


-- Convenience constructor for the most common use case: just disabling.
-- This function will help streamline its usage.
disableLinkPreviewOption :: LinkPreviewOptions
disableLinkPreviewOption = 
  LinkPreviewOptions
  { is_disabled        = True
  , url                = mempty
  , prefer_small_media = False
  , prefer_large_media = False
  , show_above_text    = False
  }

-- You'll need to define a FromJSON instance for this to parse the message_id
newtype MessageIdResponse = MessageIdResponse { message_id :: Int64 }
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
  :: Text                         -- ^ The context for logging
  -> Text                         -- ^ The message text, pre-formatted with MarkdownV2
  -> ChatKey                      -- ^ The target chat
  -> Maybe Int64                  -- ^ The target message_id
  -> Maybe Int64                  -- ^ The reply_to_message_id
  -> Maybe A.Value                -- ^ The reply_markup
  -> AppM (Either TelegramError MessageIdResponse)
sendOrEditTelegramMessage context messageText chatKey mMessageId mbReplyId mReplyMarkup = do
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
    let basePayload =
          [ Just ("chat_id" A..= chat)
          , Just ("text" A..= messageText)
          , Just ("parse_mode" A..= tshow MarkdownV2)
          , Just ("link_preview_options" A..= 
                   disableLinkPreviewOption)
          , ("reply_to_message_id" A..=) <$> mbReplyId
          , ("reply_markup" A..=)        <$> mReplyMarkup
          ]

          -- Combine the base payload with the conditional message_id field
    let payload = A.Object $ A.fromList ((catMaybes basePayload)) `A.union` messageIdField

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


deleteMessage :: Int64 -> ChatKey -> AppM (Either TelegramError ())
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


-- | Sends a photo from a local file path.
sendPhotoToTelegram :: Text -> Text -> ChatKey -> Maybe A.Value -> FilePath -> AppM (Either TelegramError MessageIdResponse)
sendPhotoToTelegram context caption chatKey mbKeyboard path = do
  -- 1. Get the necessary config from our application environment
  bots <- fmap _bots ask
  let botsInfo = M.lookup chatKey bots
  res <- for botsInfo $ \(bot, chat) -> do
    httpManager <- fmap _configHttpManager ask -- Assumes Manager is in your Config
    let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendPhoto"
    -- We need to build a multipart/form-data request
    let part = partFile "photo" path & W.partContentType .~ Just "image/jpeg"
    let payload = [ part
                  , partBS "chat_id" (TE.encodeUtf8 (T.pack (show chat)))
                  , partBS "caption" (TE.encodeUtf8 caption)
                  , partBS "parse_mode" (TE.encodeUtf8 (T.pack (show MarkdownV2)))
                  ] ++ maybe [] (\k -> [partLBS "reply_markup" (A.encode k)]) mbKeyboard

    -- Using wreq's 'post' with a list of 'Part's
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

-- | Replaces the media in an existing message. The caption is only updated if 'Just text' is provided.
editMessageMediaWithPhoto
    :: Text
    -> Int64
    -> Int64
    -> ChatKey
    -> Maybe Text  -- <<-- NEW: Caption is now optional
    -> FilePath
    -> AppM (Either TelegramError ())
editMessageMediaWithPhoto context chatId msgId chatKey maybeNewCaption filePath = do
  -- 1. Get the necessary config from our application environment
  bots <- fmap _bots ask
  let botsInfo = M.lookup chatKey bots
  res <- for botsInfo $ \(bot, chat) -> do
    httpManager <- fmap _configHttpManager ask -- Assumes Manager is in your Config
    let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/editMessageMedia"
    
    -- 1. Build the 'media' object. Aeson's 'object' with 'catMaybes'
    -- is perfect for handling optional fields like 'caption'.
    let mediaObject = A.object $ catMaybes
            [ Just ("type"  A..= ("photo" :: Text))
            , Just ("media" A..= ("attach://photo" :: Text))
            , ("caption" A..=) <$> maybeNewCaption -- This line only adds 'caption' if it's 'Just'
            ]
            
    -- 2. Build the multipart request
    let payload =
            [ W.partText "chat_id" (T.pack (show chatId))
            , W.partText "message_id" (T.pack (show msgId))
            , W.partText "media" (TE.decodeUtf8 (BL.toStrict (A.encode mediaObject)))
            , W.partFile "photo" filePath
            ]
    -- Using wreq's 'post' with a list of 'Part's
    eResult <- liftIO $ try' $ postWith (defaults & manager .~ Right httpManager) url payload
    -- 3. Process the response (simplified)
    case eResult of
        Left httpErr -> return $ Left (ApiRequestFailed (toException httpErr))
        Right response -> 
            -- Check if response body contains {"ok":true}
            if "\"ok\":true" `T.isInfixOf` (TE.decodeUtf8 . BL.toStrict $ response ^. W.responseBody)
                then return $ Right ()
                else return $ Left (TelegramApiError "Failed to edit media")
  case res of Nothing -> pure $ Left BotNotFound; Just res -> pure $ Right ();
 

sendDocument 
  :: ChatKey
  -> Text                  -- ^ The caption for the document
  -> Text                  -- ^ The filename to display in Telegram
  -> B.ByteString          -- ^ The raw binary content of the file
  -> Text
  -> AppM (Either TelegramError ())
sendDocument chatKey caption filename fileContent contentType = do
  -- 1. Get the necessary config from our application environment
  bots <- fmap _bots ask
  let botsInfo = M.lookup chatKey bots
  res <- for botsInfo $ \(bot, chat) -> do
    httpManager <- fmap _configHttpManager ask -- Assumes Manager is in your Config
    let fullUrl = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendDocument"
        
    -- Wreq's 'post' can take a list of 'Part's to build the multipart request.
    -- The other parameters are sent as form fields, not a JSON body.
     -- --- THIS IS THE FIX ---
    -- 1. Create a 'Part' manually for the in-memory file content.
    let filePart = 
          (W.partBS "document" fileContent) 
          & W.partFileName ?~ (T.unpack filename)
          & W.partContentType ?~ TE.encodeUtf8 contentType

    -- 2. Create the other parts for chat_id and caption.
    let chatPart = W.partText "chat_id" (tshow chat)
    let captionPart = W.partText "caption" caption
    let parseModePart = W.partText "parse_mode" "MarkdownV2"

    let opts = W.defaults & manager .~ Right httpManager
        -- The Content-Type header will be set automatically by wreq for multipart.
        -- We don't need to set it manually.
    
    -- 3. The 'post' function takes a list of 'Part's.
    let parts = [chatPart, captionPart, parseModePart, filePart]
    eResponse <- liftIO $ try' (postWith opts fullUrl parts)
    case eResponse of
      Left httpErr -> return $ Left (ApiRequestFailed (toException httpErr))
      Right _ -> return $ Right () 
  case res of Nothing -> pure $ Left BotNotFound; Just res -> pure res;



forwardTelegramMessage
  :: Text                         -- ^ The context for logging.
  -> ChatKey                      -- ^ The target chat to forward TO.
  -> ChatKey                      -- ^ The source chat to forward FROM.
  -> Int64                        -- ^ The message_id in the source chat to forward.
  -> AppM (Either TelegramError MessageIdResponse)
forwardTelegramMessage context toChatKey fromChaKey fromMessageId = do
  -- 1. Get the bot token and chat ID for the DESTINATION chat.
  bots <- fmap _bots ask
  let mToBotInfo = M.lookup toChatKey bots
  let mFromBotInfo = M.lookup fromChaKey bots
  
  case (,) <$> mToBotInfo <*> mFromBotInfo of
    Nothing -> pure $ Left BotNotFound
    Just ((botToken, toChatId), (_, fromChatId)) -> do
      httpManager <- fmap _configHttpManager ask
      
      -- 2. Construct the API URL for the 'forwardMessage' endpoint.
      let url = "https://api.telegram.org/bot" <> T.unpack botToken <> "/forwardMessage"
      
      -- 3. The JSON payload is simple for this endpoint.
      let payload = A.object
            [ "chat_id"      A..= toChatId
            , "from_chat_id" A..= fromChatId
            , "message_id"   A..= fromMessageId
            ]
            
      -- 4. Perform the API call (reusing your existing pattern).
      eResult <- liftIO $ try' $ postWith (defaults & manager .~ Right httpManager) url payload

      -- 5. Handle the response (reusing your existing error handling).
      case eResult of
        Right response -> do
         let mRes = A.eitherDecode @MessageIdResponse (response ^. responseBody)
         return $ either (Left . JSONError . T.pack . show) Right mRes
        Left err ->
          fmap (const (Left (ApiRequestFailed err))) $
            $(logTM) ErrorS $ "CRITICAL: Failed to forward a message for " <> ls context <> ". Error: " <> ls (show err)