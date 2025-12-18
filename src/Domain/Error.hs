 {-# LANGUAGE OverloadedStrings #-}

module Domain.Error (handleHttpError) where


import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), responseStatus)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Import your Telegram-specific error ADT if you have one
import Domain.Telegram.Types (TelegramApiError, taeDescription)

-- | Parses an HttpException and returns a descriptive Text error.
handleHttpError :: HttpException -> T.Text
handleHttpError exception =
  case exception of
    -- Case 1: The server responded with a non-2xx status code (e.g., 400, 403, 500)
    HttpExceptionRequest _ (StatusCodeException response body) ->
      let
        status = statusCode (responseStatus response)
      in
      -- Attempt to parse the JSON error body from Telegram
      case A.decode (BL.fromStrict body) :: Maybe TelegramApiError of
        Just apiError ->
          -- We have a specific error from the Telegram API
          "Telegram API Error " <> T.pack (show status) <> ": " <> (taeDescription apiError)
          
        Nothing ->
          -- We have an HTTP error, but the body wasn't the JSON we expected
          "HTTP Error " <> T.pack (show status) <> ". Could not parse error body: " <> TE.decodeUtf8 body

    -- Case 2: Network-level failures (Timeout, DNS, etc.)
    HttpExceptionRequest _ ResponseTimeout ->
      "Network Error: The request to Telegram timed out."
      
    HttpExceptionRequest _ ConnectionTimeout ->
      "Network Error: Timed out while trying to connect to Telegram."
      
    HttpExceptionRequest _ (ConnectionFailure ex) ->
      "Network Error: Could not connect to Telegram. Reason: " <> T.pack (show ex)
      
    -- Case 3: Other unexpected HTTP-related errors
    InvalidUrlException url reason ->
      "Internal Error: Invalid URL provided for HTTP request: " <> T.pack url <> " (" <> T.pack reason <> ")"
      
    -- Generic fallback for any other HttpExceptionContent constructors
    HttpExceptionRequest _ content ->
      "An unexpected HTTP error occurred: " <> T.pack (show content)