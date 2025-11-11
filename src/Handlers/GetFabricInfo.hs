{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Handlers.GetFabricInfo(handler) where

import Data.Text (Text, unpack)
import Katip (logTM, Severity(..))
import GHC.Exts (fromString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (pack)


import API.Types (FabricInfo)
import API.WithField (WithField)
import Types (AppM, AppState(appDBPool))
import API.Types (ApiResponse, mkError)
import DB (getFabricInfoById)
import Data.Bifunctor (first)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: Int -> AppM (ApiResponse (WithField "fiId" Int FabricInfo))
handler fabricId_ = do
  -- 1. Log the incoming request
  $(logTM) InfoS "Request received for fabric info"
  -- 2. Get the database connection pool from our AppState environment
  dbPool <- asks appDBPool
  -- 3. Run the database query inside our AppM monad using liftIO
  $(logTM) DebugS $ "Querying database for fabric ID: " <> fromString (show fabricId_)
  resp <- liftIO $ getFabricInfoById fabricId_ dbPool
  return $ first mkError resp