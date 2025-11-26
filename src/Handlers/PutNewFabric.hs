{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.PutNewFabric(handler) where

import Data.Text (Text, unpack)
import Katip (logTM, Severity(..), ls)
import GHC.Exts (fromString)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Data.Int (Int64)
import Control.Monad.Reader.Class (ask)
import Data.Bifunctor (first)
import Data.Traversable (for)
import Control.Monad (join, when)
import Data.Either (isLeft)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, RawIngestRequest (rawText), mkError)
import Infrastructure.Database (putNewFabric)
import Domain.Warehouse.Parser (parseIngestRequest, renderValidationErrors)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: RawIngestRequest -> AppM (ApiResponse Int64)
handler rawIngestReq = do
  -- 1. Log the incoming request
  $(logTM) DebugS "Request received for creating a new fabric"
  let eFabric = parseIngestRequest $ rawText rawIngestReq
  res <- for eFabric $ \fabric -> do
    -- 2. Get the database connection pool from our AppState environment
    pool <- fmap _appDBPool ask
    -- 3. Run the database query inside our AppM monad using liftIO
    $(logTM) DebugS $ "Querying database for making a new entry"
    dbRes <- liftIO $ putNewFabric fabric rawIngestReq pool
    when(isLeft dbRes) $ $(logTM) ErrorS $ ls $ "Error while inserting new fabric: " <> pack (show dbRes)
    return $ first (const (mkError "server error")) dbRes
  when(isLeft eFabric) $ $(logTM) ErrorS $ ls $ "Validation errors: " <> pack (show eFabric)
  return $ join $ first (mkError . renderValidationErrors) res