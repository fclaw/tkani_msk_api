{-# LANGUAGE OverloadedStrings #-}

module Text (camelToSnake, recordLabelModifier, encodeToText, pascalCase, recordLabelModifierG, tshow) where

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


recordLabelModifierG :: (String -> String) -> String -> String -> String
recordLabelModifierG modifier prefix fieldName =
  let withoutPrefix = fromMaybe fieldName (stripPrefix prefix fieldName)
  in modifier withoutPrefix

recordLabelModifier :: String -> String -> String
recordLabelModifier = recordLabelModifierG camelToSnake

-- | Converts any ToJSON instance directly to Strict Text
encodeToText :: ToJSON a => a -> Text
encodeToText val = T.replace "\"" "" $ LT.toStrict (TE.decodeUtf8 (encode val))

tshow :: Show a => a -> Text
tshow = T.pack . show
