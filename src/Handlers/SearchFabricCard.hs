{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.SearchFabricCard(handler) where

import Katip
import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Bifunctor (first)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, CatalogSummaryItem, mkError)
import Domain.Warehouse.Types (FabricType)
import Infrastructure.Database (searchFabricCard)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: FabricType -> Int64 -> AppM (ApiResponse (Maybe CatalogSummaryItem))
handler fabricType fabricId = do
  $(logTM) InfoS $ ls $ "fabric card for type: " <> show fabricType <> ", id: " <> show fabricId
  pool <- fmap _appDBPool ask
  fmap (first mkError) $ liftIO $ searchFabricCard fabricType fabricId pool
