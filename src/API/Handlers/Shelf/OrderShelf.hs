module API.Handlers.Shelf.OrderShelf (handler) where


import Data.Int (Int64)

import App (AppM)
import API.Types (ApiResponse, ShelfOrderDetails, Providers)

handler :: Int64 -> Maybe Providers -> AppM (ApiResponse ShelfOrderDetails)
handler _ _ = undefined