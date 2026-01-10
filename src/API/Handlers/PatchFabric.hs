{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.PatchFabric(handler) where

import Data.Int (Int64)
import Katip (logTM, Severity(..), ls)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack, Text)
import Data.Int (Int64)
import Control.Monad.Reader.Class (ask)
import Data.Bifunctor (first, second)
import Data.Traversable (for)
import Control.Monad (join, when)
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)


import App (AppM, _appDBPool, _thresholdMetres)
import API.Types (ApiResponse, RawIngestRequest (rawText), ApiError (ApiError), wrongModelErrorCode, NewFabric (..), mkError)
import Domain.Warehouse.Types (FabricType (..), Fabric (..))
import Infrastructure.Database (patchRoll, patchPrecut)
import Infrastructure.Database.Types (mkPatchedFabric)
import Domain.Warehouse.Parser (parseIngestRequest, renderValidationErrors, toEither)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: Int64 -> RawIngestRequest -> AppM (ApiResponse NewFabric)
handler fabricId rawIngestReq = do
  -- 1. Log the incoming request
  $(logTM) DebugS "Request received for patching a fabric"
  cfg <- ask
  let threshold = _thresholdMetres cfg
  let eFabric = toEither $ parseIngestRequest (rawText rawIngestReq) threshold
  res <- for eFabric $ \fabric -> do
    -- 2. Get the database connection pool from our AppState environment
    let pool = _appDBPool cfg
    -- 3. Run the database query inside our AppM monad using liftIO
    $(logTM) DebugS $ "Querying database for patching a fabric"
    let resp = NewFabric fabricId (fType fabric) (fromMaybe undefined (fArticle fabric)) 
    let patchedFabric = mkPatchedFabric fabricId fabric rawIngestReq
    $(logTM) DebugS $ ls $ "patched fabric: " <> show patchedFabric
    eDbRes <- 
      if fType fabric == Roll then
        patchRoll patchedFabric pool
      else patchPrecut patchedFabric pool
    return $ first (ApiError "server error") $ (second resp) eDbRes
  when(isLeft eFabric) $ $(logTM) ErrorS $ ls $ "Validation errors: " <> pack (show eFabric)
  return $ join $ first (ApiError wrongModelErrorCode . renderValidationErrors) res    