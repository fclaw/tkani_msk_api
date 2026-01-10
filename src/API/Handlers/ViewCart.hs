{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.ViewCart (handler) where


import Katip
import Data.Int (Int64)
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import GHC.List (sum)
import Data.Traversable (for)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, wrongParamsErrorCode, ApiError (..), mkError, ViewCart (..), vciPrice)
import Domain.Warehouse.Types (FabricType)
import Infrastructure.Database (fetchCartItems)


handler :: Maybe Int64 -> AppM (ApiResponse ViewCart)
handler (Just userId) = do
  eRes <- fmap (first mkError) $
    fmap _appDBPool ask >>= 
      (fetchCartItems userId)
  for eRes $ \items -> do 
    let count = length items
    let price = GHC.List.sum [ vciPrice item | item <- items]    
    return $ ViewCart count price items
handler userParam = do
  $(logTM) ErrorS $ ls $ "invalid params in Handlers.ViewCart: " <> show userParam
  return $ Left $ ApiError wrongParamsErrorCode mempty