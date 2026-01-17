{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.SetOrderDimensions (handler) where

import Data.Bifunctor (first)
import Data.Text (Text)
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
import Infrastructure.Database (setOrderDimensions)
import API.Types (ApiResponse, mkError, SetOrderDimensionsRequest)


handler :: Text -> SetOrderDimensionsRequest -> AppM (ApiResponse ())
handler orderId dimensions = fmap (first mkError) $ fmap _appDBPool ask >>= (setOrderDimensions orderId dimensions)