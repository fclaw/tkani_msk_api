{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Handlers.GetFabricPreview(handler) where

import Data.Text (Text, unpack)
import Katip (logTM, Severity(..), ls)
import GHC.Exts (fromString)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import Data.Int (Int64)


import API.Types (FabricPreview, ApiError (..), wrongParamsErrorCode)
import App (AppM, _appDBPool, _thresholdMetres)
import Control.Monad.Reader.Class (ask)
import API.Types (ApiResponse, mkError, errorCode)
import Infrastructure.Database (getFabricPreview)
import Data.Bifunctor (first)
import Domain.Warehouse.Types (FabricType)

-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: Maybe Int64 -> Maybe FabricType -> AppM (ApiResponse FabricPreview)
handler (Just fabricId) (Just fabricType) = do
  -- -- 1. Log the incoming request
  $(logTM) InfoS "Request received for fabric preview"
  -- 2. Get the database connection pool from our AppState environment
  cfg <- ask
  let pool = _appDBPool cfg
  let thresholdMetres =  _thresholdMetres cfg
  -- 3. Run the database query inside our AppM monad using liftIO
  $(logTM) DebugS $ ls $ "Querying database for fabric ID: " <> show fabricId <> ", fabric type: " <> show fabricType
  eResp <- liftIO $ getFabricPreview fabricId fabricType thresholdMetres pool
  case eResp of 
    Right (Right fabricInfo) -> pure $ Right fabricInfo
    Right (Left err) -> do
      $(logTM) ErrorS $ 
        "Fabric not found for ID: " <> 
        fromString (show fabricId) <> 
        ", error: " <> 
        fromString (unpack err)
      pure $ Left $ (mkError err) { errorCode = "404" }
    Left err -> do
      $(logTM) ErrorS $ 
        "Database error while fetching fabric ID: " <> 
        fromString (show fabricId) <> 
        ", error: " <> 
        fromString (unpack err)
      pure $ Left $ mkError err
handle fabricParam typeParam = do
  $(logTM) ErrorS $ ls $ "invalid params in Handlers.GetFabricPreview: " <> show fabricParam <> ", " <> show typeParam
  return $ Left $ ApiError wrongParamsErrorCode mempty