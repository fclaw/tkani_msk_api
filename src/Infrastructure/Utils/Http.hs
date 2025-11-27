{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Infrastructure.Utils.Http
  ( 
    -- Updated signatures: They now all take a 'Manager'
    getReq,
    postReq,
    postFormReq,
    
    makeRequestWithRetries,

    -- Primitive API (Now takes Manager)
    _getReq',
    _postReq',
    _postFormReq',

    handleApiResponse,
    handleWorkerApiResponse,
    withRetry,
    HttpError(..),
    QueryParams,
    FormParams
  )
where

import           Control.Lens           ((^.), (.~), (&))
import           Data.Aeson             (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.Wreq           (Response, defaults, getWith, params,
                                         postWith, responseBody, header, FormParam (..), manager) -- ADDED manager
import qualified Data.Text.Encoding     as TE
import           Servant                (ServerError, err500, errBody)
import           Control.Monad.Error.Class
import           Katip
import           Network.HTTP.Client             (HttpException (..),
                                                  HttpExceptionContent (..),
                                                  responseStatus, Manager) -- ADDED Manager
import qualified Network.HTTP.Client          as HTTP
import           Network.HTTP.Types.Status       (statusCode)
import           Control.Exception               (SomeException, fromException, try, Exception)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString.Char8 as BS8
import           Control.Exception.Lifted (throwIO)
import           Control.Concurrent (threadDelay)
import           Control.Monad.Base (liftBase)
import           Control.Monad.Trans.Control (MonadBaseControl)

type QueryParams = [(Text, Text)]
type FormParams = [FormParam]

data HttpError = NetworkError SomeException | JsonDecodeError Text
  deriving (Show)

instance Exception HttpError

perseReq body =
  case eitherDecode body of
    Left err -> Left $ JsonDecodeError (T.pack err)
    Right decodedBody -> Right decodedBody    

perseResp :: FromJSON a => Either SomeException (Response LBS.ByteString) -> Either HttpError a
perseResp eResp =
  case eResp of
    Left ex -> Left $ NetworkError ex
    Right response -> perseReq (response ^. responseBody)

addToken Nothing opt = opt
addToken (Just token) opts = opts & header "Authorization" .~ [TE.encodeUtf8 ("Bearer " <> token)]

data HttpExceptionInfo
  = RetryableNetworkError HttpExceptionContent
  | RetryableServerError Int
  | AuthTokenExpired
  | ClientError Int
  | UnclassifiedException SomeException

classifyException :: SomeException -> HttpExceptionInfo
classifyException ex =
  case fromException ex of
    Just httpException ->
      case httpException of
        HttpExceptionRequest _ content ->
          case content of
            ConnectionTimeout      -> RetryableNetworkError content
            ConnectionFailure _    -> RetryableNetworkError content
            ResponseTimeout        -> RetryableNetworkError content
            ConnectionClosed       -> RetryableNetworkError content
            StatusCodeException response body ->
              let status = statusCode (responseStatus response)
              in 
                if status == 401 then AuthTokenExpired
                else if status >= 500 && status < 600 then RetryableServerError status
                else ClientError status
            _ -> UnclassifiedException ex
        InvalidUrlException _ _ -> UnclassifiedException ex
    Nothing -> UnclassifiedException ex

retryWithBackoff
  :: (KatipContext m, Catch.MonadCatch m)
  => Maybe (m ())
  -> Int
  -> Int
  -> m (Either SomeException a)
  -> m (Either SomeException a)
retryWithBackoff _ 0 _ action = action
retryWithBackoff mRecoveryAction retries delay action = do
  eResult <- action
  case eResult of
    Right result -> pure (Right result)
    Left ex ->
      case classifyException ex of
        AuthTokenExpired ->
          case mRecoveryAction of
            Nothing -> pure (Left ex)
            Just recoveryAction -> do
              $(logTM) WarningS "Auth token expired. Refreshing..."
              eRecoveryResult <- Catch.try recoveryAction
              case eRecoveryResult of
                Left (recoveryEx :: SomeException) -> pure (Left recoveryEx)
                Right () -> retryWithBackoff mRecoveryAction (retries - 1) delay action
        RetryableNetworkError _ -> do
          liftIO $ threadDelay delay
          retryWithBackoff mRecoveryAction (retries - 1) (delay * 2) action
        RetryableServerError _ -> do
          liftIO $ threadDelay delay
          retryWithBackoff mRecoveryAction (retries - 1) (delay * 2) action
        ClientError _ -> pure (Left ex)
        UnclassifiedException _ -> pure (Left ex)

maxRetries :: Int
maxRetries = 3

initialDelay :: Int
initialDelay = 1000000 

-- ===================================================================
-- == 1. PRIMITIVE REQUESTS (UPDATED to accept Manager)
-- ===================================================================

_getReq' :: (KatipContext m, MonadIO m, Catch.MonadCatch m) => Manager -> String -> QueryParams -> Maybe Text -> m (Either SomeException (Response LBS.ByteString))
_getReq' mgr url queryParams maybeToken = do
  -- FIX: Use global manager
  let baseOpts = defaults & manager .~ Right mgr
  let opts = addToken maybeToken (baseOpts & params .~ queryParams)
  liftIO $ try (getWith opts url)

_postReq' :: (KatipContext m, MonadIO m, Catch.MonadCatch m, ToJSON b) => Manager -> String -> b -> Maybe Text -> m (Either SomeException (Response LBS.ByteString))
_postReq' mgr url body maybeToken = do
  -- FIX: Use global manager and set Content-Type
  let baseOpts = defaults 
        & manager .~ Right mgr 
        & header "Content-Type" .~ [BS8.pack "application/json"]

  let opts = addToken maybeToken baseOpts
  let encoded_body = encode body
  liftIO $ try (postWith opts url encoded_body)

_postFormReq' :: (KatipContext m, MonadIO m, Catch.MonadCatch m) => Manager -> String -> FormParams -> m (Either SomeException (Response LBS.ByteString))
_postFormReq' mgr url payload = do
  -- FIX: Use global manager
  let opts = defaults & manager .~ Right mgr
  liftIO $ try (postWith opts url payload)


-- ===================================================================
-- == 2. PUBLIC API (UPDATED to accept Manager)
-- ===================================================================

getReq :: forall a m. (KatipContext m, MonadIO m, Catch.MonadCatch m, FromJSON a) => Manager -> String -> QueryParams -> Maybe Text -> m (Either HttpError a)
getReq mgr url queryParams maybeToken = do
  makeRequestWithRetries Nothing (_getReq' mgr url queryParams maybeToken)

postReq :: forall a b m. (KatipContext m, MonadIO m, Catch.MonadCatch m, FromJSON a, ToJSON b) => Manager -> String -> b -> Maybe Text -> m (Either HttpError a)
postReq mgr url body maybeToken = do
  makeRequestWithRetries Nothing (_postReq' mgr url body maybeToken)

postFormReq :: forall a m. (KatipContext m, MonadIO m, Catch.MonadCatch m, FromJSON a) => Manager -> String -> FormParams -> m (Either HttpError a)
postFormReq mgr url payload = do
  makeRequestWithRetries Nothing (_postFormReq' mgr url payload)

-- ... (Rest of adapters handleApiResponse, etc. remain the same) ...

handleApiResponse
  :: ( KatipContext m, MonadError ServerError m)
  => Text
  -> Either HttpError a
  -> (a -> m b)
  -> m b
handleApiResponse callName eResult onSUCCESS =
  case eResult of
    Left err -> do
      let errorMsg = "Failed API call to '" <> callName <> "': "
      $(logTM) ErrorS $ logStr (errorMsg <> T.pack (show err))
      throwError err500 { errBody = "External API call failed. See logs for details." }
    Right successPayload -> onSUCCESS successPayload

handleWorkerApiResponse 
    :: forall a m . (FromJSON a, KatipContext m)
    => Text
    -> Either HttpError a
    -> (HttpError -> m ())
    -> (a -> m ())
    -> m ()
handleWorkerApiResponse callName (Left ex) onError _ = do
  let errorMsg = "Worker API call to '" <> callName <> "' failed: "
  $(logTM) ErrorS $ logStr (errorMsg <> T.pack (show ex))
  onError ex
handleWorkerApiResponse _ (Right val) _ onSuccess = onSuccess val

showt :: Show a => a -> Text
showt = T.pack . show

makeRequestWithRetries
  :: forall a m. (KatipContext m, Catch.MonadCatch m, MonadIO m, FromJSON a)
  => Maybe (m ())
  -> m (Either SomeException (Response LBS.ByteString))
  -> m (Either HttpError a)
makeRequestWithRetries mRecoveryAction httpAction = do
  eResponse <- retryWithBackoff mRecoveryAction maxRetries initialDelay httpAction
  pure $ perseResp eResponse

withRetry :: forall m a . (MonadBaseControl IO m, KatipContext m) => Int -> m (Either HttpError a) -> m a
withRetry attempts action = go 1
  where
    go attempt = do
        result <- action
        case result of
            Right val -> return val
            Left err -> do
                if attempt >= attempts
                then do
                      $(logTM) ErrorS $ ls $
                         "Retry limit reached (" <> showt attempts <> " attempts). Exception: " <> showt err 
                      throwIO err 
                else do
                      $(logTM) WarningS $ ls $
                        "Attempt " <> showt attempt <> "/" <> showt attempts <> " failed. Retrying... "
                      liftBase $ threadDelay 2000000 
                      go (attempt + 1)