{-# LANGUAGE OverloadedStrings   #-}

module API.Handlers.Shelf.EditShelfPersonalInfo (handler) where

import Katip
import Data.Int (Int64)
import Data.Bifunctor (first)
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
import Infrastructure.Database (editShelfPersonalInfo)
import API.Types(ApiResponse, ShelfPersonalInfo (..), mkError)


handler :: Int64 -> ShelfPersonalInfo -> AppM (ApiResponse ())
handler userId personalInfo = do
  pool <- fmap _appDBPool ask
  fmap (first (const (mkError "server error"))) $ editShelfPersonalInfo userId personalInfo pool