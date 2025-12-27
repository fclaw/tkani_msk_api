{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.PutNewFabric(handler) where

import Data.Text (Text, unpack)
import Katip (logTM, Severity(..), ls)
import GHC.Exts (fromString)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack, Text)
import Data.Int (Int64)
import Control.Monad.Reader.Class (ask)
import Data.Bifunctor (first, second)
import Data.Traversable (for)
import Control.Monad (join, when)
import Data.Either (isLeft)
import Data.Maybe (isJust)

import App (AppM, _appDBPool, _thresholdMetres)
import API.Types (ApiResponse, RawIngestRequest (rawText), mkError, errorCode, ApiError (ApiError), wrongModelErrorCode, NewFabric (..))
import Infrastructure.Database (putNewFabric, checkFabricPreCuts)
import Domain.Warehouse.Parser (parseIngestRequest, renderValidationErrors, toEither)
import Domain.Warehouse.Types (Fabric (..), FabricType (..))
import Utils.Telegram.Markdown (escapeMarkdownV2)


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x1, x2, x3) = f x1 x2 x3


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: RawIngestRequest -> AppM (ApiResponse NewFabric)
handler rawIngestReq = do
  -- 1. Log the incoming request
  $(logTM) DebugS "Request received for creating a new fabric"
  cfg <- ask
  let threshold = _thresholdMetres cfg
  let eFabric = toEither $ parseIngestRequest (rawText rawIngestReq) threshold
  res <- for eFabric $ \fabric -> do
    -- 2. Get the database connection pool from our AppState environment
    let pool = _appDBPool cfg
    -- 3. Run the database query inside our AppM monad using liftIO
    $(logTM) DebugS $ "Querying database for making a new entry"

    -- check compatibility, for the fabric cut-to-order cannot override being sold in pre-cuts
    -- yes-no sql if for the given article there is a fabric being sold in pre-cuts  
    if fType fabric == Roll && isJust(fArticle fabric) then do
      let Just article = fArticle fabric
      $(logTM) DebugS $ ls @Text $ 
        "Checking compatibility for cut-to-order fabric with article: " <> 
        fromString (unpack article)
      eHasPreCuts <- liftIO $ checkFabricPreCuts article pool
      case eHasPreCuts of 
        Left errDb -> do
          $(logTM) ErrorS $ ls  @Text $ 
            "Database error while checking pre-cuts for article " <> 
            fromString (unpack article) <> 
            ", error: " <> 
            fromString (unpack errDb)
          return $ Left $ mkError "server error"
        Right hasPreCuts -> 
          if hasPreCuts then do 
            $(logTM) ErrorS $ ls  @Text $ 
              "Incompatibility detected: fabric with article " <> 
              fromString (unpack article) <> 
              " is already sold in Pre-Cuts. Cannot add as Cut-to-Order."
            return $ Left $ (mkError $
              escapeMarkdownV2 $ 
                "Incompatibility detected: fabric with article " <> 
                article <>
                " is already sold in Pre-Cuts. Cannot add as Cut-to-Order.") 
              { errorCode = "400" }
          else do
            dbRes <- liftIO $ putNewFabric fabric rawIngestReq pool
            when(isLeft dbRes) $ $(logTM) ErrorS $ ls $ "Error while inserting new fabric: " <> pack (show dbRes)
            let mkNewFabric id art isGallery = NewFabric id (fType fabric) art isGallery
            return $ first (const (mkError "server error")) $ (second (uncurry3 mkNewFabric)) dbRes
      else do
        dbRes <- liftIO $ putNewFabric fabric rawIngestReq pool
        when(isLeft dbRes) $ $(logTM) ErrorS $ ls $ "Error while inserting new fabric: " <> pack (show dbRes)
        let mkNewFabric id art isGallery = NewFabric id (fType fabric) art isGallery
        return $ first (const (mkError "server error")) $ (second (uncurry3 mkNewFabric)) dbRes 
  when(isLeft eFabric) $ $(logTM) ErrorS $ ls $ "Validation errors: " <> pack (show eFabric)
  return $ join $ first (ApiError wrongModelErrorCode . renderValidationErrors) res