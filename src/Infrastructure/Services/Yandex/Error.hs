{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}

module Infrastructure.Services.Yandex.Error (getError, getHttpException) where

import           Data.Aeson          (FromJSON, decodeStrict)
import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding as T
import           GHC.Generics        (Generic)
import           Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), responseStatus)
import           Network.HTTP.Types  (statusCode)
import           Control.Exception   (SomeException, fromException)

import           Infrastructure.Utils.Http (HttpError (..))


-- | ADT to match Yandex's error JSON structure: {"code": "...", "message": "..."}
data YandexErrorBody = YandexErrorBody
  { code    :: Text
  , message :: Text
  } deriving (Show, Generic)

instance FromJSON YandexErrorBody

-- | Extracts a human-readable error from a network exception.
-- Attempts to parse Yandex-specific JSON if it's a StatusCodeException.
getError :: HttpException -> Text
getError (InvalidUrlException url reason) = 
  "Invalid URL: " <> T.pack url <> " (" <> T.pack reason <> ")"

getError (HttpExceptionRequest _ content) = case content of
  -- This handles 400, 403, 404, 409 etc. where Yandex returns a JSON body
  StatusCodeException resp body -> 
    let codeInt = statusCode (responseStatus resp)
    in case decodeStrict body :: Maybe YandexErrorBody of
      -- If JSON matches Yandex format: "[errorCode] message"
      Just err -> "[" <> code err <> "] " <> message err
      -- Fallback if JSON is malformed or different
      Nothing  -> "Server returned " <> T.pack (show codeInt) <> ": " <> T.decodeUtf8 body

  -- General Network Failures
  ResponseTimeout    -> "The request timed out. Please check your internet or API status."
  ConnectionFailure _ -> "Connection failure. The Yandex API server is unreachable."
  NoResponseDataReceived -> "The server closed the connection without sending data."
  
  -- Fallback for all other HttpExceptionContent types (TlsException, etc.)
  other -> T.pack (show other)


-- | Conveniently attempts to "narrow" the error down to an HttpException.
getHttpException :: HttpError -> Maybe HttpException
getHttpException = \case
  -- If it's a NetworkError, we use fromException to see if it's actually 
  -- an HttpException (vs a DB error or other SomeException).
  NetworkError someExc     -> fromException someExc
  
  -- JsonDecodeErrors are logical application failures, 
  -- so they never contain a raw HttpException.
  JsonDecodeError _ _      -> Nothing