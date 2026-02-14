{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.UploadMediaForFabric (handler) where


import Data.Bifunctor (first)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, mkError, FabricMediaRequest (..))
import Infrastructure.Database (addMediaToFabric)


handler :: FabricMediaRequest -> AppM (ApiResponse ())
handler req = fmap (first mkError) $ fmap _appDBPool ask >>= (addMediaToFabric req)
{-# INLINE handler #-}