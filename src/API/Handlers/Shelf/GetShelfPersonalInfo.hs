{-# LANGUAGE OverloadedStrings   #-}

module API.Handlers.Shelf.GetShelfPersonalInfo (handler) where


import Katip
import Data.Int (Int64)
import Data.Bifunctor (bimap)
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
import Infrastructure.Database (getShelfPersonalInfo)
import API.Types(ApiResponse, ShelfPersonalInfo (..), mkError)


handler :: Int64 -> AppM (ApiResponse ShelfPersonalInfo)
handler userId = do
  pool <- fmap _appDBPool ask
  fmap (bimap (const (mkError "server error")) (uncurry ShelfPersonalInfo)) $ getShelfPersonalInfo userId pool