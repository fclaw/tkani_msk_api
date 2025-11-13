{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Http
  ( getReq,
    postReq,
    HttpError(..),
    QueryParams,
  )
where

import           Control.Exception      (SomeException, try)
import           Control.Lens           ((^.), (.~), (&))
import           Data.Aeson             (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.Wreq           (Response, defaults, getWith, params,
                                         postWith, responseBody)

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

-- | Performs a GET request and decodes the JSON response into the specified type.
--   Usage: result <- getReq @MyResponseType "http://api.example.com/data"
getReq :: forall a. FromJSON a => String -> QueryParams -> IO (Either HttpError a)
getReq url queryParams = do
  -- Start with default options
  -- The (.~) operator is from the 'lens' library, used by wreq to set options.
  -- It reads as "set the 'params' part of 'defaults' to 'queryParams'".
  let opts = defaults & params .~ queryParams
  -- wreq will automatically handle URL encoding for the parameter values.
  eResponse <- try (getWith opts url) :: IO (Either SomeException (Response LBS.ByteString))
  return $ perseResp eResponse

-- | Performs a POST request with a JSON body and decodes the JSON response.
--   Usage: result <- postReq @MyResponseType "http://api.example.com/create" myPayload
postReq :: forall a b. (FromJSON a, ToJSON b) => String -> b -> IO (Either HttpError a)
postReq url body = do
  let opts = defaults
  eResponse <- try (postWith opts url (encode body)) :: IO (Either SomeException (Response LBS.ByteString))
  return $ perseResp eResponse