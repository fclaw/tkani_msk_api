module Text (camelToSnake, recordLabelModifier) where

import Data.Char (toLower, isUpper)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

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