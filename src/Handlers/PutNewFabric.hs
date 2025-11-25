{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.PutNewFabric(handler) where

import Data.Text (Text, unpack)
import Katip (logTM, Severity(..))
import GHC.Exts (fromString)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Data.Int (Int64)
import Control.Monad.Reader.Class (ask)
import Data.Bifunctor (first)
import Data.Traversable (for)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, RawIngestRequest (rawText), mkError)
import Infrastructure.Database (putNewFabric)
import Domain.Warehouse.Parser (parseIngestRequest)



-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: RawIngestRequest -> AppM (ApiResponse Int64)
handler rawIngestReq = do
  -- 1. Log the incoming request
  $(logTM) DebugS "Request received for creating a new fabric"
  let parsedText = parseIngestRequest $ rawText rawIngestReq
  res <- for parsedText $ \fabric -> do
    -- 2. Get the database connection pool from our AppState environment
    -- dbPool <- fmap _appDBPool ask
    -- 3. Run the database query inside our AppM monad using liftIO
    $(logTM) DebugS $ "Querying database for making a new entry"
    return 1
  return $ undefined res