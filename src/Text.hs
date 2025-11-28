{-# LANGUAGE OverloadedStrings #-}

module Text (camelToSnake, recordLabelModifier, encodeToText, pascalCase) where

import Data.Char (toLower, toUpper, isUpper)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (ToJSON, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x:xs) = toLower x : go xs
  where
    go [] = []
    go (y:ys)
      | isUpper y = '_' : toLower y : go ys
      | otherwise   = y : go ys

-- | Converts a string to PascalCase.
--   Example: "terminalKey" -> "TerminalKey"
pascalCase :: String -> String
pascalCase (c:cs) = toUpper c : cs
pascalCase mempty = mempty


recordLabelModifier :: String -> String -> String
recordLabelModifier prefix fieldName =
  let withoutPrefix = fromMaybe fieldName (stripPrefix prefix fieldName)
  in camelToSnake withoutPrefix

-- | Converts any ToJSON instance directly to Strict Text
encodeToText :: ToJSON a => a -> Text
encodeToText val = T.replace "\"" "" $ LT.toStrict (TE.decodeUtf8 (encode val))