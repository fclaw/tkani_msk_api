module Handlers.GetCatalogSummary(handler) where


import API.Types (CatalogDate, CatalogSummary)
import App (AppM)
import API.Types (ApiResponse)

handler :: Maybe CatalogDate -> AppM (ApiResponse CatalogSummary)
handler mbDate = undefined