module Handlers.DailyDigest.Draft (handler) where


import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)

import App (AppM, _appDBPool)
import API.Types (DailyDigestDraft, ApiResponse, mkError)
import Infrastructure.Database (updateDailyDigestDraft)


handler :: DailyDigestDraft -> AppM (ApiResponse ())
handler draft = do
  pool <- fmap _appDBPool ask
  fmap (first mkError) $ liftIO $ updateDailyDigestDraft draft pool