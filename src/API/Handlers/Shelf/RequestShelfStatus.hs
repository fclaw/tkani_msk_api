{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}

module API.Handlers.Shelf.RequestShelfStatus (handler) where


import Katip
import Data.Int (Int64)
import Data.Bifunctor (bimap)
import Control.Monad.Reader.Class (ask)

import Text (tshow)
import App (AppM, _appDBPool)
import Infrastructure.Database (getShelfStatus)
import API.Types(ApiResponse, ShelfStatusResponse (..), mkError)


handler :: Int64 -> AppM (ApiResponse ShelfStatusResponse)
handler userId = do
  pool <- fmap _appDBPool ask
  fmap (bimap (const (mkError "server error")) ShelfStatusResponse) $ getShelfStatus userId pool