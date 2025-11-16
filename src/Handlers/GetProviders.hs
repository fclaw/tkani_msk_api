{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.GetProviders(handler) where

import API.Types (ProviderInfo (..))
import App (AppM, _providers)
import Control.Monad.Reader.Class (ask)
import API.Types (ApiResponse)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: AppM (ApiResponse [ProviderInfo])
handler = fmap (Right . _providers) $ ask