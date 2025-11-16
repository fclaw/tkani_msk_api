{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.PutNewFabric(handler) where

import Data.Text (Text, unpack)
import Katip (logTM, Severity(..))
import GHC.Exts (fromString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (pack)

import API.Types (FabricInfo)
import App (AppM, _appDBPool)
import Control.Monad.Reader.Class (ask)
import API.Types (ApiResponse, FabricInfo, mkError)
import Infrastructure.Database (putNewFabric)
import Data.Bifunctor (first)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: FabricInfo -> AppM (ApiResponse Int)
handler newFabricInfo_ = do
  -- 1. Log the incoming request
  $(logTM) DebugS "Request received for creating a new fabric"
  -- 2. Get the database connection pool from our AppState environment
  dbPool <- fmap _appDBPool ask
  -- 3. Run the database query inside our AppM monad using liftIO
  $(logTM) DebugS $ "Querying database for making a new entry"
  resp <- liftIO $ putNewFabric newFabricInfo_ dbPool
  return $ first mkError resp