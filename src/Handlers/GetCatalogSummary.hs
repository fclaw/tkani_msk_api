{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.GetCatalogSummary(handler) where


import Katip
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Coerce (coerce)
import Data.Text (pack)
import Data.Time (Day)
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M

import API.Types (CatalogDate (..), CatalogSummary (..))
import App (AppM)
import API.Types (ApiResponse, mkError, csiWarehouseChatId)
import Infrastructure.Database (fetchCatalogSummaryItem)
import App (AppM, _appDBPool, _bots, ChatKey (WAREHOUSE))


handler :: Maybe CatalogDate -> AppM (ApiResponse CatalogSummary)
handler Nothing = return $ Left $ mkError "Date parameter is required"
handler (Just cday) = do
  let day = (coerce cday) :: Day
  $(logTM) InfoS $ ls $ "Request received for fetching catalog items for " <> (pack (iso8601Show day))
  cfg <- ask
  let pool = _appDBPool cfg
  let Just (_, chatId) = M.lookup WAREHOUSE $ _bots cfg
  eRes <- liftIO $ fetchCatalogSummaryItem day pool
  let catalogSummary =
        flip fmap eRes $ \items ->
          let newItems = 
                flip map items $ \item -> 
                  item { csiWarehouseChatId = fromIntegral chatId }
          in CatalogSummary 
              (pack (iso8601Show day))
              (length newItems)
              newItems
  return $ first mkError catalogSummary
