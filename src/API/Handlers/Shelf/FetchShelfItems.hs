{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}

module API.Handlers.Shelf.FetchShelfItems (handler) where


import Katip
import Data.Int (Int64)
import Data.Functor ((<&>))
import Control.Monad.Reader.Class (ask)
import Data.Time.Clock (diffUTCTime, nominalDay)

import Text (tshow)
import App (AppM, _appDBPool, currentTime, _shelfCapacity)
import Infrastructure.Database (fetchShelfItems)
import API.Types(ApiResponse, ShelfItemsResponse (..), ApiError (..), wrongParamsErrorCode, mkError)


handler :: Int64 -> AppM (ApiResponse ShelfItemsResponse)
handler userId = do
  pool <- fmap _appDBPool ask
  dbRes <- fetchShelfItems userId pool
  case dbRes of
    Left err ->
      fmap (const (Left (mkError "server error"))) $
        $(logTM) ErrorS $ 
          "insertion of a new shelf \
          \ record has resulted in error " <> 
          ls (tshow err)
    Right Nothing -> pure $ Left (mkError "shelf not found")
    Right (Just (maybeTm, items)) -> do
      curr <- currentTime
      capacity <- fmap _shelfCapacity ask
      pure $ Right $ ShelfItemsResponse capacity items $ maybeTm <&> \tm -> floor (diffUTCTime curr tm / nominalDay)