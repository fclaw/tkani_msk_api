{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Handlers.GetFabricInfo(handler) where

import Data.Text (Text, unpack)
import Katip (logTM, Severity(..))
import GHC.Exts (fromString)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)


import API.Types (FullFabric)
import API.WithField (WithField)
import App (AppM, _appDBPool)
import Control.Monad.Reader.Class (ask)
import API.Types (ApiResponse, mkError)
import Infrastructure.Database (getFabricInfoById)
import Data.Bifunctor (first)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: Int -> AppM (ApiResponse FullFabric)
handler fabricId_ = do
  -- 1. Log the incoming request
  $(logTM) InfoS "Request received for fabric info"
  -- 2. Get the database connection pool from our AppState environment
  dbPool <- fmap _appDBPool ask
  -- 3. Run the database query inside our AppM monad using liftIO
  $(logTM) DebugS $ "Querying database for fabric ID: " <> fromString (show fabricId_)
  resp <- liftIO $ getFabricInfoById fabricId_ dbPool
  return $ first mkError resp