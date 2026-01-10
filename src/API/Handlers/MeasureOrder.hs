{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.MeasureOrder (handler) where

import Data.Bifunctor (bimap)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, mkError, MeasureRequest (..), MeasureResponse (..))
import Infrastructure.Database (markedOrderAsMeasured)


handler :: MeasureRequest -> AppM (ApiResponse MeasureResponse)
handler req = fmap (bimap mkError MeasureResponse) $ fmap _appDBPool ask >>= (markedOrderAsMeasured (mrTrackingNumber req))