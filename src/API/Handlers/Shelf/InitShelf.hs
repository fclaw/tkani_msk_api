{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}

module API.Handlers.Shelf.InitShelf (handler) where

import Katip
import Data.Int (Int64)
import Data.Bifunctor (bimap)
import Control.Monad (void)
import Control.Monad.Reader.Class (ask)
import qualified Data.HashMap.Strict as HM

import Text (tshow)
import API.WithField (WithField (..))
import TH.Location (currentModule)
import Infrastructure.Database (initShelf, InitShelf (..))
import Utils.Telegram.Markdown (escapeMarkdownV2)
import App (AppM, _appDBPool, forkAppM, ChatKey (SHELF), render, _totalShelves)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import API.Types(ApiResponse, ShelfRequest (..), mkError, ShelfIdResponse (..), ShelfIdResponseStatus (..))

handler :: (WithField "user_id" Int64 ShelfRequest) -> AppM (ApiResponse ShelfIdResponse)
handler (WithField userId shelfRequest) = do
  $(logTM) InfoS $ "request for new shelf received " <> ls (tshow shelfRequest)
  pool <- fmap _appDBPool ask
  totalShelves <- fmap _totalShelves ask
  eDbRes <- initShelf userId totalShelves shelfRequest pool
  $(logTM) DebugS $ "init shelf result: " <> ls (tshow eDbRes)
  case eDbRes of
    Left err ->
      fmap (const (Left (mkError "server error"))) $
        $(logTM) ErrorS $ 
          "insertion of a new shelf \
          \ record has resulted in error " <> 
          ls (tshow err)
    Right ShelfAlready -> pure $ Right $ ShelfIdResponse Nothing Already
    Right ShelfCapacityExceeded -> pure $ Right $ ShelfIdResponse Nothing CapacityExceeded
    Right (ShelfSuccess shelfId) -> do
      fmap (const (Right (ShelfIdResponse (Just shelfId) Ok))) $
        forkAppM $ do
          $(logTM) InfoS $
           "forked to send shelf details to telegram " <>
           ls (tshow shelfId)
          let placeholders = 
                HM.fromList 
                [ ("shelf_id", tshow shelfId)
                , ("telegram_user_id", tshow userId)
                , ("user_phone", srPhone shelfRequest)
                , ("user_initials", srInitials shelfRequest)
                , ("region", srRegion shelfRequest)
                ]
          shelfMessage <- fmap escapeMarkdownV2 $ render $currentModule placeholders
          void $ sendOrEditTelegramMessage mempty shelfMessage SHELF Nothing Nothing Nothing