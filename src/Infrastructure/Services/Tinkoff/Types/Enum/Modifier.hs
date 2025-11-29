{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Tinkoff.Types.Enum.Modifier (modifier) where

import Data.Char (isUpper, toLower) -- for our helper

-- | Converts a String from PascalCase to snake_case, with special handling.
modifier :: String -> String
modifier name = case name of
    -- Special cases that don't follow the snake_case rule
    "OSN"  -> "osn"
    "ENVD" -> "envd"
    "ESN"  -> "esn"
    -- General rule: convert PascalCase to snake_case
    _ -> toSnake name
  where
    -- A simple implementation of PascalCase to snake_case
    toSnake :: String -> String
    toSnake [] = []
    toSnake (x:xs) = toLower x : foldr go "" xs
      where
        go c acc = if isUpper c then '_' : toLower c : acc else c : acc