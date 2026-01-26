{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}

module API.Handlers.Shelf.InitShelf (handler) where

import Katip
import Data.Int (Int64)
import Data.Bifunctor (bimap)
import Control.Monad.Reader.Class (ask)

import Text (tshow)
import App (AppM, _appDBPool)
import API.WithField (WithField (..))
import Infrastructure.Database (initShelf)
import API.Types(ApiResponse, ShelfRequest, ShelfIdResponse, mkError, ShelfIdResponse (..))

handler :: (WithField "user_id" Int64 ShelfRequest) -> AppM (ApiResponse ShelfIdResponse)
handler (WithField userId shelfRequest) = do
  $(logTM) InfoS $ "request for new shelf received " <> ls (tshow shelfRequest)
  pool <- fmap _appDBPool ask
  eDbRes <- initShelf userId shelfRequest pool
  case eDbRes of
    Left err ->
      fmap (const (Left (mkError "server error"))) $
        $(logTM) ErrorS $ 
          "insertion of a new shelf \
          \ record has resulted in error " <> 
          ls (tshow err)
    Right Nothing -> pure $ Left (mkError "user already has a shelf")
    Right (Just shelfId) -> pure $ Right (ShelfIdResponse shelfId)