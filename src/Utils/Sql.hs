{-# LANGUAGE OverloadedStrings #-}

module Utils.Sql (splitSql) where

import Data.Text (Text)
import qualified Data.Text as T

-- | A simple utility to split a multi-command SQL string into a list of individual commands.
--   It handles basic comments and whitespace.
splitSql :: Text -> [Text]
splitSql = filter (not . T.null) . map T.strip . T.splitOn ";"