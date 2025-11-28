{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Services.Tinkoff.Security
  ( generatedToken
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
  , tokenDescription :: Text -- <-- ADDED: Now optional
  , tokenTerminalKey :: Text
  , tokenSecret      :: Text
  } deriving (Show)

-- | Generates the SHA-256 token required by the Tinkoff API.
generatedToken :: Token -> Text
generatedToken Token{..} =
    let
        -- 1. Create a list of potential key-value pairs
        -- We use a list of Maybe's to handle the optional Description field
        maybePairs :: [(Text, Text)]
        maybePairs =
            [ ("Amount",      tokenAmount)
            , ("Description", tokenDescription)
            , ("OrderId",     tokenOrderId)
            , ("Password",    tokenSecret)
            , ("TerminalKey", tokenTerminalKey)
            ]

        -- 2. Create the final map from the valid pairs
        -- 'catMaybes' filters out any Nothing values from the list.
        valueMap :: Map Text Text
        valueMap = Map.fromList maybePairs

        -- 3. Prepend the secret password to the concatenated sorted values
        stringToSign = T.concat (Map.elems valueMap)

        -- 4. Convert to ByteString for hashing
        byteStringToSign = TE.encodeUtf8 stringToSign

        -- 5. Hash with SHA-256
        digest :: Digest SHA256 = hash byteStringToSign

        -- 6. Encode to lowercase hexadecimal Text
        hexEncodedHash = TE.decodeUtf8 $ Base16.encode $ convert digest

    in T.toLower hexEncodedHash