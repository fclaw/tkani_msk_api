{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.CheckCartItem (handler) where


import Katip
import Data.Int (Int64)
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first, second)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, CheckItemInCart (..), wrongParamsErrorCode, ApiError (..), mkError)
import Domain.Warehouse.Types (FabricType)
import Infrastructure.Database (isItemInCart)


handler :: Maybe Int64 -> Maybe Int64 -> Maybe FabricType -> AppM (ApiResponse CheckItemInCart)
handler (Just userId) (Just fabricId) (Just fabricType) =
  fmap (first mkError . second CheckItemInCart) $
    fmap _appDBPool ask >>= (isItemInCart userId fabricType fabricId)
handler userParam fabricParam typeParam = do
  $(logTM) ErrorS $ ls $ "invalid params in Handlers.CheckCartItem: " <> show userParam <> ", " <> show fabricParam <> ", " <> show typeParam
  return $ Left $ ApiError wrongParamsErrorCode mempty