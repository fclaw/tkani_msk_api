{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.ToggleExtraDiscount (handler) where

import Katip
import Data.Int (Int64)
import Data.Bifunctor (first)
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, mkError, ToggleExtraDiscountRequest (..))
import Infrastructure.Database (toggleExtraDiscount)


handler :: Int64 -> ToggleExtraDiscountRequest -> AppM (ApiResponse ())
handler itemId req = fmap (first mkError) $ fmap _appDBPool ask >>= (toggleExtraDiscount itemId (tedrIsEnabled req))