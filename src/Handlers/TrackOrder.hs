{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.TrackOrder(handler) where

import Katip (logTM, Severity(..), ls)
import Control.Monad.Reader.Class (ask)
import Data.Aeson (Result (..))
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import Control.Monad (when, join)
import Data.Bifunctor (bimap)
import Data.Text (Text, pack)
import Data.Traversable (for)

import App (AppM, _appDBPool)
import Control.Monad.Reader.Class (ask)
import API.Types (ApiResponse, TrackOrder (..), mkError, formatStatus)
import Infrastructure.Database (fetchOrderStatus)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: Maybe Text -> AppM (ApiResponse (Maybe TrackOrder))
handler mQuery =
  fmap (fmap join . sequence) $ for mQuery $ \query -> do
    -- 1. Log the incoming request
    $(logTM) DebugS "Request received for getting the status"
    -- 2. Get the database connection pool from our AppState environment
    pool <- fmap _appDBPool ask
    res <- liftIO $ fetchOrderStatus query pool
    when(isLeft res) $ $(logTM) ErrorS $ ls $ "aeson error " <> pack (show res)
    let convert (st, orderId, trackingN, provider) = 
          TrackOrder (formatStatus st) orderId trackingN provider
    return $ bimap mkError (fmap convert) res