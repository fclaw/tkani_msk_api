module Handlers.DailyDigest.Publish (handler) where

import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)

import App (AppM, _appDBPool)
import API.Types (DailyDigestPublish, ApiResponse, mkError)
import Infrastructure.Database (publishDailyDigest)


handler :: DailyDigestPublish -> AppM (ApiResponse ())
handler dailyDigest = do
  pool <- fmap _appDBPool ask
  fmap (first mkError) $ liftIO $ publishDailyDigest dailyDigest pool