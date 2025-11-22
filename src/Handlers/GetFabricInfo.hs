{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Handlers.GetFabricInfo(handler) where

import Data.Text (Text, unpack)
import Katip (logTM, Severity(..))
import GHC.Exts (fromString)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Data.Int (Int64)


import API.Types (FullFabric)
import API.WithField (WithField)
import App (AppM, _appDBPool, _thresholdMetres)
import Control.Monad.Reader.Class (ask)
import API.Types (ApiResponse, mkError, errorCode)
import Infrastructure.Database (getFabricInfoById)
import Data.Bifunctor (first)

-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: Int64 -> AppM (ApiResponse FullFabric)
handler fabricId = do
  -- 1. Log the incoming request
  $(logTM) InfoS "Request received for fabric info"
  -- 2. Get the database connection pool from our AppState environment
  cfg <- ask
  let pool = _appDBPool cfg
  let thresholdMetres =  _thresholdMetres cfg
  -- 3. Run the database query inside our AppM monad using liftIO
  $(logTM) DebugS $ "Querying database for fabric ID: " <> fromString (show fabricId)
  eResp <- liftIO $ getFabricInfoById fabricId thresholdMetres pool
  liftIO $ print eResp
  return $ case eResp of 
    Right (Right fabricInfo) -> Right fabricInfo
    Right (Left err) -> Left $ (mkError err) { errorCode = "404" }
    Left err -> Left $ mkError err