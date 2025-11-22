module Text (camelToSnake, recordLabelModifier, encodeToText) where

import Data.Char (toLower, isUpper)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (ToJSON, encode)
import Data.Text (Text)
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



recordLabelModifier :: String -> String -> String
recordLabelModifier prefix fieldName =
  let withoutPrefix = fromMaybe fieldName (stripPrefix prefix fieldName)
  in camelToSnake withoutPrefix

-- | Converts any ToJSON instance directly to Strict Text
encodeToText :: ToJSON a => a -> Text
encodeToText val = LT.toStrict (TE.decodeUtf8 (encode val))