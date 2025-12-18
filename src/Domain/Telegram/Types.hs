{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Domain.Telegram.Types where

import Data.Aeson.TH
import Data.Text (Text)

import Text (camelToSnake, recordLabelModifierG)

-- | Represents a standard error response from the Telegram Bot API.
--   Example JSON: {"ok":false, "error_code":400, "description":"..."}
data TelegramApiError = TelegramApiError
  { -- | The HTTP-like status code provided by Telegram (e.g., 400, 403).
    taeErrorCode   :: Int
    -- | The human-readable description of the error.
  , taeDescription :: Text
  } deriving (Show, Eq)


$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifierG camelToSnake "tae"} ''TelegramApiError)