{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.GetCatalogSummaryV2 (handler) where


import Katip
import Data.Bifunctor (first)
import Control.Monad.Reader.Class (ask)
import qualified Data.Map.Strict as M

import App (AppM)
import Text (tshow)
import API.Types (CatalogSummaryV2 (..))
import Domain.Warehouse.Enums (FabricLifecycle)
import API.Types (ApiResponse, mkError, csiWarehouseChatId)
import Infrastructure.Database (fetchCatalogSummaryItemV2)
import App (AppM, _appDBPool, _bots, ChatKey (WAREHOUSE), _thresholdMetres)


handler :: Maybe FabricLifecycle -> AppM (ApiResponse CatalogSummaryV2)
handler Nothing = return $ Left $ mkError "FabricLifecycle is required"
handler (Just lifeCycle) = do
  $(logTM) InfoS $ ls $ "Request received for fetching catalog items for " <> tshow lifeCycle
  cfg <- ask
  let pool = _appDBPool cfg
  let threshold = _thresholdMetres cfg
  let Just (_, chatId) = M.lookup WAREHOUSE $ _bots cfg
  eRes <- fetchCatalogSummaryItemV2 lifeCycle chatId threshold pool
  let catalogSummary = 
        flip fmap eRes $ \items -> 
          CatalogSummaryV2 (length items) items
  return $ first mkError catalogSummary
