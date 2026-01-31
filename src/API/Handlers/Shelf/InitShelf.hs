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
import Infrastructure.Database (initShelf)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import App (AppM, _appDBPool, forkAppM, ChatKey (SHELF), render)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import API.Types(ApiResponse, ShelfRequest (..), mkError, ShelfIdResponse (..))

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
    Right (Just shelfId) -> do
      fmap (const (Right (ShelfIdResponse shelfId))) $
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
                ]
          shelfMessage <- fmap escapeMarkdownV2 $ render $currentModule placeholders
          void $ sendOrEditTelegramMessage mempty shelfMessage SHELF Nothing Nothing Nothing