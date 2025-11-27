{-# LANGUAGE OverloadedStrings #-}

module Handlers.GetCatalogSummary(handler) where


import API.Types (CatalogDate, CatalogSummary)
import App (AppM)
import API.Types (ApiResponse, mkError)

handler :: Maybe CatalogDate -> AppM (ApiResponse CatalogSummary)
handler Nothing = return $ Left $ mkError "Date parameter is required"
handler (Just date) = undefined