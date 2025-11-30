{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.SearchFabrics (handler) where

import Katip
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Bifunctor (first)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, SearchTeaser, mkError)
import Infrastructure.Database (searchFabrics)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: Maybe Text -> AppM (ApiResponse [SearchTeaser])
handler Nothing = return $ Right []
handler (Just query) = do
  $(logTM) InfoS $ ls $ "Request received for search, query: " <> query
  cfg <- ask
  let pool = _appDBPool cfg
  fmap (first mkError) $ liftIO $ searchFabrics query 50 pool