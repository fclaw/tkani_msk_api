{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.SetTelegramMessage(handler) where

import Katip (logTM, Severity(..), ls)
import GHC.Exts (fromString)
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)

import API.Types (ApiResponse, SetTelegramMessageRequest, mkError)
import App (AppM, _appDBPool)
import Infrastructure.Database (setTelegramMessage)
import Data.Bifunctor (bimap)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: SetTelegramMessageRequest -> AppM (ApiResponse ())
handler message = do
  -- 1. Log the incoming request
  $(logTM) DebugS "(SetTelegramMessageRequest): Request received for creating a new message"
  $(logTM) DebugS $ ls $ "(SetTelegramMessageRequest): message: " <>  pack (show message)
  -- 2. Get the database connection pool from our AppState environment
  dbPool <- fmap _appDBPool ask
  -- 3. Run the database query inside our AppM monad using liftIO
  $(logTM) DebugS $ "(SetTelegramMessageRequest): Querying database for making a new entry"
  resp <- liftIO $ setTelegramMessage message dbPool
  $(logTM) DebugS $ ls $ "(SetTelegramMessageRequest): Querying database for making a new entry resp: " <> pack (show resp)
  return $ bimap mkError (const ()) resp