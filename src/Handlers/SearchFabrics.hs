{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.SearchFabrics (handler) where

import Katip
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Bifunctor (first)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, SearchTeaser, mkError)
import Infrastructure.Database (searchFabrics)


-- 1. Function to sanitize and format input for Postgres TSQuery
--    Input: "  шерсть   burber  "
--    Output: "шерсть:* & burber:*"
prepareTsQuery :: T.Text -> T.Text
prepareTsQuery input = 
    let 
        -- Split by spaces, remove garbage
        wordsList = filter (not . T.null) $ T.words $ T.strip input
        -- Append ":*" to every word to enable Prefix Matching
        -- "Dior" -> "Dior:*"
        wildcarded = map (<> ":*") wordsList
    in T.intercalate " & " wildcarded

-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: Maybe Text -> AppM (ApiResponse [SearchTeaser])
handler Nothing = return $ Right []
handler (Just query) | T.length query < 3 = return $ Right []
handler (Just query) = do
  let prepQuery = prepareTsQuery query
  $(logTM) InfoS $ ls $ "Request received for search, prepared query: " <> prepQuery
  pool <- fmap _appDBPool ask
  fmap (first mkError) $ liftIO $ searchFabrics prepQuery 50 pool