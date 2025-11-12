{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.GetProviders(handler) where

import Control.Monad.Reader (asks)

import API.Types (ProviderInfo (..))
import Types (AppM, AppState(providers))
import API.Types (ApiResponse)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: AppM (ApiResponse [ProviderInfo])
handler = fmap Right $ asks providers