{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.DeleteFabric (handler) where

import Data.Int (Int64)
import Data.Bifunctor (first)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, mkError)
import Domain.Warehouse.Types (FabricType (..))
import Infrastructure.Database (deleteFabric)


handler :: Int64 -> FabricType -> AppM (ApiResponse ())
handler fabricId fabricType = fmap (first mkError) $ fmap _appDBPool ask >>= (deleteFabric fabricId fabricType)