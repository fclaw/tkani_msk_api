{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.ToggleExtraDiscount (handler) where

import Katip
import Data.Int (Int64)
import Data.Bifunctor (bimap)
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, mkError, ToggleExtraDiscountRequest (..), ToggleExtraDiscountResponse (..))
import Infrastructure.Database (toggleExtraDiscount)


handler :: Int64 -> ToggleExtraDiscountRequest -> AppM (ApiResponse ToggleExtraDiscountResponse)
handler itemId req = fmap (bimap mkError ToggleExtraDiscountResponse) $ fmap _appDBPool ask >>= (toggleExtraDiscount itemId (tedrIsEnabled req))