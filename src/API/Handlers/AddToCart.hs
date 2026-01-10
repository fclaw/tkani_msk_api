{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.AddToCart (handler) where


import Katip
import Data.Int (Int64)
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Either (isLeft)
import Control.Monad (when)
import qualified Hasql.Session as Hasql
import qualified Hasql.Pool as Hasql
import qualified Data.Text as T
import qualified Data.ByteString as B

import App (AppM, _appDBPool, _cutTolerance)
import API.Types (ApiResponse, CartNewFabric, mkError, ApiError(..), cartLimitExceeded, CartCheckStatus)
import Infrastructure.Database (addToCart)


handler :: CartNewFabric -> AppM (ApiResponse CartCheckStatus)
handler item = do
  cfg <- ask
  let pool = _appDBPool cfg
  let cutTolerance = fromIntegral (_cutTolerance cfg) / 100.0
  eRes <- addToCart item cutTolerance pool
  when(isLeft eRes) $ $(logTM) ErrorS $ ls $ "db error: " <> show eRes
  return $ first handleCartDbError eRes

handleCartDbError :: Hasql.UsageError -> ApiError
handleCartDbError (Hasql.SessionUsageError sessionError) = 
  case sessionError of
    (Hasql.QueryError _ _ (Hasql.ResultError (Hasql.ServerError "P0001" msg _ _ _))) -> do
      -- Check if the message is the one we're looking for
      if "Cart item limit reached" `B.isInfixOf` msg
      then ApiError cartLimitExceeded "Cart is full."
      else mkError $ T.pack (show sessionError)
    error -> mkError $ T.pack (show error)
handleCartDbError error = mkError $ T.pack (show error)