{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.ClearCart (handler) where


import Katip
import Data.Int (Int64)
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first, second)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, wrongParamsErrorCode, ApiError (..), mkError)
import Domain.Warehouse.Types (FabricType)
import Infrastructure.Database (clearCart)


handler :: Maybe Int64 -> AppM (ApiResponse ())
handler (Just userId) = 
  fmap (first mkError) $ 
    fmap _appDBPool ask >>= 
      (clearCart userId)
handler userParam = do
  $(logTM) ErrorS $ ls $ "invalid params in Handlers.ClearCart: " <> show userParam
  return $ Left $ ApiError wrongParamsErrorCode mempty