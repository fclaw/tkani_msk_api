{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Services.Tinkoff.Security
  ( generatedToken
  , generateGetStateToken
  , GetStateToken (..)
  , Token(..)
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
data Token = Token
  { tokenAmount      :: Text
  , tokenOrderId     :: Text
  , tokenDescription :: Maybe Text -- <-- ADDED: Now optional
  , tokenTerminalKey :: Text
  , tokenSecret      :: Text
  } deriving (Show)

-- | Generates the SHA-256 token required by the Tinkoff API.
generatedToken :: Token -> Text
generatedToken Token{..} =
    let
        -- 1. Create a list of the key-value pairs FOR SIGNING.
        maybePairs :: [Maybe (Text, Text)]
        maybePairs =
            [ Just ("Amount",      tokenAmount)
            , ("Description",) <$> tokenDescription
            , Just ("OrderId",     tokenOrderId)
            , Just ("TerminalKey", tokenTerminalKey)
            -- THIS IS THE FIX: The key must be "Password"
            , Just ("Password",    tokenSecret)
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