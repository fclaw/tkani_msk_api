{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils.Http
  ( getReq,
    postReq,
    postFormReq,
    handleApiResponse,
    HttpError(..),
    QueryParams,
    FormParams
  )
where

import           Control.Exception      (SomeException, try)
import           Control.Lens           ((^.), (.~), (&))
import           Data.Aeson             (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.Wreq           (Response, defaults, getWith, params,
                                         postWith, responseBody, header, FormParam (..))
import qualified Data.Text.Encoding     as TE
import           Servant                (ServerError, err500, errBody)
import           Control.Monad.Error.Class
import           Katip
import           Network.HTTP.Client             (HttpException (..),
                                                  HttpExceptionContent (..),
                                                  responseStatus)
import           Network.HTTP.Types.Status       (statusCode)
import           Control.Exception               (SomeException, fromException, try)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Concurrent (threadDelay)


-- A cleaner way to represent query parameters
type QueryParams = [(Text, Text)]

data HttpError = NetworkError Text | JsonDecodeError Text
  deriving (Show)


perseReq body =
  case eitherDecode body of
    Left err -> Left $ JsonDecodeError (T.pack err)
    Right decodedBody -> Right decodedBody    

perseResp eResp =
  case eResp of
    Left ex -> Left $ NetworkError (T.pack $ show ex)
    Right response -> perseReq (response ^. responseBody)

addToken Nothing opt = opt
addToken (Just token) opts = opts & header "Authorization" .~ [TE.encodeUtf8 ("Bearer " <> token)]


-- | A custom ADT to classify HTTP exceptions for clearer handling.
data HttpExceptionInfo
  -- | A transient network error that is safe to retry.
  = RetryableNetworkError HttpExceptionContent
  -- | A server-side error (5xx) that is safe to retry. Holds the status code.
  | RetryableServerError Int
  -- | A client-side error (4xx) that should NOT be retried. Holds the status code.
  | ClientError Int
  -- | Any other type of exception, which we will not retry.
  | UnclassifiedException SomeException


-- | Classifies a generic SomeException into our specific HttpExceptionInfo ADT.
classifyException :: SomeException -> HttpExceptionInfo
classifyException ex =
  -- fromException gives us the top-level HttpException
  case fromException ex of
    -- We match on the main constructor to get the 'content' field.
    Just (HttpExceptionRequest _ content) ->
      -- NOW we can analyze the specific reason (the HttpExceptionContent).
      case content of
        -- These are the retryable network errors.
        ConnectionTimeout      -> RetryableNetworkError content
        ConnectionFailure _    -> RetryableNetworkError content
        ResponseTimeout        -> RetryableNetworkError content
        ConnectionClosed       -> RetryableNetworkError content

        -- The StatusCodeException is a constructor for HttpExceptionContent,
        -- so we correctly match it here in the nested case.
        StatusCodeException response _ ->
          let status = statusCode (responseStatus response)
          in if status >= 500 && status < 600
               then RetryableServerError status
               else ClientError status

        -- Any other HttpExceptionContent (like InvalidHeader) is not something we want to retry.
        _ -> UnclassifiedException ex

    -- Handle other types of HttpException (like InvalidUrlException) or
    -- a completely different exception type. Neither should be retried.
    _ -> UnclassifiedException ex

retryWithBackoff
  :: (KatipContext m, MonadIO m)
  => Int
  -> Int
  -> m (Either SomeException a)
  -> m (Either SomeException a)
retryWithBackoff 0 _ action = action
retryWithBackoff retries delay action = do
  eResult <- action
  case eResult of
    Right result -> pure (Right result) -- Success!
    Left ex ->
      -- Use our new classifier!
      case classifyException ex of
        RetryableNetworkError content -> do
          $(logTM) WarningS $ logStr $
            "Network error (" <> T.pack (show content) <> "), retrying in " <>
            T.pack (show (delay `div` 1000000)) <> "s... (" <> T.pack (show (retries - 1)) <> " retries left)"
          liftIO $ threadDelay delay
          retryWithBackoff (retries - 1) (delay * 2) action

        RetryableServerError status -> do
          $(logTM) WarningS $ logStr $
            "Server error (HTTP " <> show status <> "), retrying in " <>
            show (delay `div` 1000000) <> "s... (" <> show (retries - 1) <> " retries left)"
          liftIO $ threadDelay delay
          retryWithBackoff (retries - 1) (delay * 2) action

        -- These are the non-retryable cases.
        ClientError _               -> pure (Left ex)
        UnclassifiedException _     -> pure (Left ex)

-- Define retry constants in one place
maxRetries :: Int
maxRetries = 3

initialDelay :: Int
initialDelay = 1000000 -- 1 second

-- | Performs a GET request with retries and decodes the JSON response.
getReq :: forall a m. (KatipContext m, MonadIO m, FromJSON a) => String -> QueryParams -> Maybe Text -> m (Either HttpError a)
getReq url queryParams maybeToken = do
  let opts = addToken maybeToken (defaults & params .~ queryParams)
  -- The ONLY change is wrapping this line with our retry helper
  eResponse <- retryWithBackoff maxRetries initialDelay (liftIO $ try (getWith opts url))
  
  pure $ perseResp eResponse

-- | Performs a POST request with retries and a JSON body.
postReq :: forall a b m. (KatipContext m, MonadIO m, FromJSON a, ToJSON b) => String -> b -> Maybe Text -> m (Either HttpError a)
postReq url body maybeToken = do
  let opts = addToken maybeToken defaults
  let encoded_body = encode body
  -- The ONLY change is wrapping this line
  eResponse <- retryWithBackoff maxRetries initialDelay (liftIO $ try (postWith opts url encoded_body))
  pure $ perseResp eResponse

-- A type alias for form parameters to keep signatures clean
type FormParams = [FormParam]

-- | Performs a POST request with retries and a form-urlencoded body.
postFormReq :: forall a m. (KatipContext m, MonadIO m, FromJSON a) => String -> FormParams -> m (Either HttpError a)
postFormReq url payload = do
  -- The ONLY change is wrapping this line
  eResponse <- retryWithBackoff maxRetries initialDelay (liftIO $ try (postWith defaults url payload))
  pure $ perseResp eResponse


-- | A higher-order function in Continuation-Passing Style to handle the
--   result of an API call made with our http helpers.
--   It abstracts away the boilerplate of error logging and throwing.
handleApiResponse
  :: ( KatipContext m
     , MonadError ServerError m)
  => Text                       -- ^ A descriptive name for the API call (for logging)
  -> Either HttpError a         -- ^ The result from getReqAuth or similar
  -> (a -> m b)                 -- ^ The "Continuation": a function to run on success
  -> m b
handleApiResponse callName eResult onSUCCESS =
  case eResult of
    Left err -> do
      let errorMsg = "Failed API call to '" <> callName <> "': "
      $(logTM) ErrorS $ logStr (errorMsg <> "error: " <> T.pack (show err))
      throwError err500 { errBody = "External API call failed. See logs for details." }
    -- If the result was Right, simply call the success continuation
    -- with the unwrapped payload.
    Right successPayload -> onSUCCESS successPayload