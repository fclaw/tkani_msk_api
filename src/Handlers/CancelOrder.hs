{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell #-}

module Handlers.CancelOrder (handler) where

import Katip
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Control.Monad.Reader.Class (ask)
import Data.Traversable (for)
import Data.Foldable (for_)
import Control.Monad (when)
import Data.Either (isLeft)

import App (AppM, _appDBPool, _tinkoffCred, tinkoffTerminalKey, tinkoffSecret)
import API.Types (ApiResponse, CancelOrder, mkError, coOrderId)
import Infrastructure.Database (fetchPaymentId)
import Infrastructure.Services.Tinkoff (cancelTinkoffPayment)
import Infrastructure.Services.Tinkoff.Types.Cancel
import Infrastructure.Services.Tinkoff.Security (generateCancelToken, CancelToken (..))


handler :: CancelOrder -> AppM (ApiResponse ())
handler cancel = do
  cfg <- ask
  let pool = _appDBPool cfg
  eRes <- fmap (first mkError) $ liftIO $ fetchPaymentId (coOrderId cancel) pool
  for eRes $ \maybePaymentId ->
    for_ maybePaymentId $ \paymentId -> do
      let tinkoffCred = _tinkoffCred cfg
      let key = tinkoffTerminalKey tinkoffCred
      let secret = tinkoffSecret tinkoffCred
      let token = CancelToken paymentId key secret
      let cancelReq = CancelRequest key paymentId (generateCancelToken token)
      eRes <- cancelTinkoffPayment cancelReq
      when (isLeft eRes) $  $(logTM) ErrorS $ ls $ "(Tinkoff) Cancel order has finished with error: " <> show eRes
        