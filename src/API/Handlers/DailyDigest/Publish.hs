module API.Handlers.DailyDigest.Publish (handler) where

import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)

import App (AppM, _appDBPool)
import API.Types (DailyDigest, ApiResponse, mkError)
import Infrastructure.Database (setDailyDigestStatus)
import Infrastructure.Database.Types (DailyDigestStatus (Ready))


handler :: DailyDigest -> AppM (ApiResponse ())
handler dailyDigest = do
  pool <- fmap _appDBPool ask
  fmap (first mkError) $ setDailyDigestStatus dailyDigest Ready pool