{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Http
  ( getReq,
    postReq,
    postFormReq, 
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


-- | Performs a GET request and decodes the JSON response into the specified type.
--   Usage: result <- getReq @MyResponseType "http://api.example.com/data"
getReq :: forall a. FromJSON a => String -> QueryParams -> Maybe Text -> IO (Either HttpError a)
getReq url queryParams maybeToken = do
  -- Start with default options
  -- The (.~) operator is from the 'lens' library, used by wreq to set options.
  -- It reads as "set the 'params' part of 'defaults' to 'queryParams'".
  let opts = addToken maybeToken (defaults & params .~ queryParams)
  -- wreq will automatically handle URL encoding for the parameter values.
  eResponse <- try (getWith opts url) :: IO (Either SomeException (Response LBS.ByteString))
  return $ perseResp eResponse

-- | Performs a POST request with a JSON body and decodes the JSON response.
--   Usage: result <- postReq @MyResponseType "http://api.example.com/create" myPayload
postReq :: forall a b. (FromJSON a, ToJSON b) => String -> b -> Maybe Text -> IO (Either HttpError a)
postReq url body maybeToken = do
  let opts = addToken maybeToken defaults
  let encoded_body = encode body
  eResponse <- try (postWith opts url encoded_body) :: IO (Either SomeException (Response LBS.ByteString))
  return $ perseResp eResponse

-- A type alias for form parameters to keep signatures clean
type FormParams = [FormParam]

-- | Performs a POST request with an application/x-www-form-urlencoded body.
--   This is typically used for OAuth2 token requests.
postFormReq :: forall a. FromJSON a => String -> FormParams -> IO (Either HttpError a)
postFormReq url payload = do
  -- When the payload is of type [FormParam], wreq automatically sets the
  -- Content-Type to application/x-www-form-urlencoded.
  eResponse <- try (postWith defaults url payload) :: IO (Either SomeException (Response LBS.ByteString))
  -- We can reuse our existing response parser!
  pure $ perseResp eResponse