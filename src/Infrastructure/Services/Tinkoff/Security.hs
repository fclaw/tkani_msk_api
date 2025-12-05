{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Services.Tinkoff.Security
  ( generatedInitToken
  , generateGetStateToken
  , generateGetQrToken
  , generateCancelToken
  , GetStateToken (..)
  , InitToken(..)
  , GetQrToken (..)
  , CancelToken (..)
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as Base16
import           Crypto.Hash (hash, Digest, SHA256)
import           Data.ByteArray (convert)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)

-- | Data required to generate the Tinkoff API signature token.
data InitToken = InitToken
  { itAmount      :: Text
  , itOrderId     :: Text
  , itDescription :: Maybe Text -- <-- ADDED: Now optional
  , itTerminalKey :: Text
  , itSecret      :: Text
  } deriving (Show)

-- | Generates the SHA-256 token required by the Tinkoff API.
generatedInitToken :: InitToken -> Text
generatedInitToken InitToken{..} =
    let
        -- 1. Create a list of the key-value pairs FOR SIGNING.
        maybePairs :: [Maybe (Text, Text)]
        maybePairs =
            [ Just ("Amount",      itAmount)
            , ("Description",) <$> itDescription
            , Just ("OrderId",     itOrderId)
            , Just ("TerminalKey", itTerminalKey)
            -- THIS IS THE FIX: The key must be "Password"
            , Just ("Password",    itSecret)
            ]
        
        -- Filter out Nothing values (in case Description is empty)
        valueMap :: Map Text Text
        valueMap = Map.fromList (catMaybes maybePairs)

        -- 2. Concatenate the sorted values. The Map ensures alphabetical key order.
        -- Keys: Amount, Description, OrderId, TerminalKey
        concatenatedValues = T.concat (Map.elems valueMap)

        -- 3. Prepend the secret password to the concatenated string.
        -- THIS IS THE CRUCIAL STEP FOR THE 'securepay' ENDPOINT.
        stringToSign = concatenatedValues

        -- 4. Hash the resulting string
        byteStringToSign = TE.encodeUtf8 stringToSign
        digest :: Digest SHA256 = hash byteStringToSign
        hexEncodedHash = TE.decodeUtf8 $ Base16.encode $ convert digest

    in T.toLower hexEncodedHash

data GetStateToken = GetStateToken
  { gstPaymentId   :: Text -- PaymentId as Text for signing
  , gstTerminalKey :: Text
  , gstSecret      :: Text
  }

-- This is a NEW signing function
generateGetStateToken :: GetStateToken -> Text
generateGetStateToken GetStateToken{..} =
    let
        -- For GetState, it's a simpler set of fields
        valueMap = Map.fromList
            [ ("PaymentId",   gstPaymentId)
            , ("TerminalKey", gstTerminalKey)
            , ("Password",    gstSecret)
            ]
        
        stringToSign = T.concat (Map.elems valueMap)
        
        digest :: Digest SHA256 = hash (TE.encodeUtf8 stringToSign)
        hexEncodedHash = TE.decodeUtf8 $ Base16.encode $ convert digest
    
    in T.toLower hexEncodedHash

-- NEW: Data for GetQr token
data GetQrToken = GetQrToken
  { gqrPaymentId   :: Text
  , gqrTerminalKey :: Text
  , gqrSecret      :: Text
  , gqrDataType    :: Text
  }

-- NEW: Function to generate the token for GetQr
generateGetQrToken :: GetQrToken -> Text
generateGetQrToken GetQrToken{..} =
    let
        -- 1. Create the map with ONLY the required fields for GetQr
        valueMap :: Map Text Text
        valueMap = Map.fromList
            [ ("DataType",    gqrDataType)
            , ("PaymentId",   gqrPaymentId)
            , ("TerminalKey", gqrTerminalKey)
            , ("Password",    gqrSecret)
            ]

        -- 2. Concatenate the sorted values (Password, PaymentId, TerminalKey)
        stringToSign = T.concat (Map.elems valueMap)
        
        -- 3. Hash and encode
        digest :: Digest SHA256 = hash (TE.encodeUtf8 stringToSign)
    in T.toLower $ TE.decodeUtf8 $ Base16.encode $ convert digest

data CancelToken = CancelToken
  { cPaymentId   :: Text -- PaymentId as Text for signing
  , cTerminalKey :: Text
  , cSecret      :: Text
  }

-- This is a NEW signing function
generateCancelToken :: CancelToken -> Text
generateCancelToken CancelToken{..} =
    let
        -- For GetState, it's a simpler set of fields
        valueMap = Map.fromList
            [ ("PaymentId",   cPaymentId)
            , ("TerminalKey", cTerminalKey)
            , ("Password",    cSecret)
            ]
        
        stringToSign = T.concat (Map.elems valueMap)
        
        digest :: Digest SHA256 = hash (TE.encodeUtf8 stringToSign)
        hexEncodedHash = TE.decodeUtf8 $ Base16.encode $ convert digest
    
    in T.toLower hexEncodedHash