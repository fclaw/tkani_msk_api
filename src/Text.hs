{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Text 
       ( camelToSnake
       , recordLabelModifier
       , encodeToText
       , pascalCase
       , firstToLower
       , recordLabelModifierG
       , tshow
       , textToInt
       , textToDouble
       , textToBool
       , textMoneyToDouble) 
       where

import Data.Char (toLower, toUpper, isUpper)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (ToJSON, encode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE
import Data.Text.Read (signed, decimal, double)
import Text.Read (readMaybe)


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

firstToLower :: String -> String
firstToLower [] = []
firstToLower (first:rest) = toLower first : rest


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

-- | A more performant way to convert Text to Int, avoiding the String conversion.
textToInt :: Text -> Maybe Integer
textToInt t =
  case signed decimal t of
    -- The parser succeeded and consumed the ENTIRE string
    Right (intValue, "") -> Just intValue
    -- The parser succeeded but there was leftover text (e.g., for "123a")
    Right (_, _rest)     -> Nothing
    -- The parser failed
    Left _               -> Nothing

-- | A more performant way to convert Text to Double.
textToDouble :: Text -> Maybe Double
textToDouble t =
  -- First, normalize the decimal separator
  let normalizedText = T.replace "," "." t
  in
  case signed double normalizedText of
    -- Ensure the entire string was consumed
    Right (doubleValue, "") -> Just doubleValue
    -- Any other result is a failure
    _                       -> Nothing

textToBool :: Text -> Bool
textToBool = read @Bool . T.unpack

-- | Safely parses a currency string (like "390.00") into a Double.
--   Returns 'Just Double' on success, and 'Nothing' if parsing fails.
--   It also handles decimal commas by replacing them with periods.
textMoneyToDouble :: Text -> Maybe Double
textMoneyToDouble moneyText =
    -- Replace comma with period for robustness, then unpack to String
    let preparedString = T.unpack $ T.replace "," "." moneyText
    in readMaybe preparedString