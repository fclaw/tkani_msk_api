{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.SearchFabrics (handler) where

import Katip
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)

import App (AppM, _appDBPool, _thresholdMetres)
import API.Types (ApiResponse, SearchTeaser, mkError, PaginatedResults (..), defPaginatedResults)
import Infrastructure.Database (searchFabrics)
import Text (tshow)


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
handler 
  :: Maybe Text -- "query"
  -> Maybe Int  -- "page"
  -> Maybe Int  -- "limit"
  -> AppM (ApiResponse (PaginatedResults SearchTeaser))
handler Nothing _ _ = return $ Right defPaginatedResults
handler (Just query) _ _ | T.length query < 3 = return $ Right defPaginatedResults
handler (Just query) maybePage maybeLimit = do
  -- 1. Set Defaults for pagination
  let page = fromMaybe 1 maybePage
  let limit = fromMaybe 10 maybeLimit
  -- Calculate offset: For page 1, offset is 0. For page 2, offset is 10 (if limit is 10).
  let offset = (page - 1) * limit
  let prepQuery = prepareTsQuery query
  let paginatedResults = defPaginatedResults { prPage = page, prLimit = limit }
  $(logTM) InfoS $ ls $ "Request received for search, prepared query: " <> prepQuery
  cfg <- ask
  let pool = _appDBPool cfg
  let threshold = _thresholdMetres cfg
  fmap (first mkError) $ do
    eItems <- searchFabrics prepQuery limit offset threshold pool
    $(logTM) InfoS $ ls $ "found " <> tshow (length eItems) <> " for query: " <> prepQuery
    return $ flip fmap eItems $ \(total, teasers) ->
      -- 3. Construct the final response object
      let totalPages = (total + limit - 1) `div` limit
      in paginatedResults { prItems = teasers, prTotal = total, prTotalPages = totalPages }