{-# LANGUAGE OverloadedStrings #-}

module Utils.Telegram.Markdown (escapeMarkdownV2) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set

-- | A set of all characters that must be escaped for Telegram's MarkdownV2.
specialChars :: Set Char
specialChars = Set.fromList "_*[]()~`>#+-=|{}.!"

-- | Escapes a Text value for safe use in Telegram's MarkdownV2 format.
--   It prepends a backslash '\' to any special character.
escapeMarkdownV2 :: Text -> Text
escapeMarkdownV2 = T.concatMap escapeChar
  where
    -- This function is applied to every character in the input Text.
    escapeChar :: Char -> Text
    escapeChar c
      -- If the character is in our set of special chars...
      | c `Set.member` specialChars = T.pack ['\\', c] -- ...return it with a backslash.
      -- Otherwise...
      | otherwise                   = T.singleton c   -- ...return the character as is.