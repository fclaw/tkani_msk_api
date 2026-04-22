{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.CancelOrder (handler) where

import Katip
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Control.Monad.Reader.Class (ask)
import Data.Traversable (for)
import Data.Foldable (for_)
import Control.Monad (when, void)
import Data.Either (isLeft)

import Text (tshow)
import App (AppM, _appDBPool, _tinkoffCred, tinkoffTerminalKey, tinkoffSecret, ChatKey (ORDER))
import API.Types (ApiResponse, CancelOrder, mkError, coOrderId)
import Infrastructure.Database (fetchPaymentId, cancelConfirmedOrder)
import Infrastructure.Services.Tinkoff (cancelTinkoffPayment)
import Infrastructure.Services.Tinkoff.Types.Cancel
import Infrastructure.Services.Tinkoff.Security (generateCancelToken, CancelToken (..))
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Utils.Telegram.Markdown (escapeMarkdownV2)


handler :: CancelOrder -> AppM (ApiResponse ())
handler cancel = do
  cfg <- ask
  let pool = _appDBPool cfg
  eRes <- fmap (first mkError) $ fetchPaymentId (coOrderId cancel) pool
  for eRes $ \maybePaymentId ->
    for_ maybePaymentId $ \paymentId -> do
      let tinkoffCred = _tinkoffCred cfg
      let key = tinkoffTerminalKey tinkoffCred
      let secret = tinkoffSecret tinkoffCred
      let token = CancelToken paymentId key secret
      let cancelReq = CancelRequest key paymentId (generateCancelToken token)
      eRes <- cancelTinkoffPayment cancelReq
      for_ eRes $ const $ do 
        eDbRes <- cancelConfirmedOrder (coOrderId cancel) pool
        for_ eDbRes $ \amount -> do
          let cancelMsg = 
                "order " <> 
                coOrderId cancel <> 
                " has been cancelled, " <> 
                tshow amount <> 
                " RUB has been returned to the customer"
          void $ sendOrEditTelegramMessage mempty (escapeMarkdownV2 cancelMsg) ORDER Nothing Nothing Nothing
        when (isLeft eDbRes) $  $(logTM) ErrorS $ ls $ "cancelConfirmedOrder has finished with error: " <> show eDbRes
      when (isLeft eRes) $  $(logTM) ErrorS $ ls $ "(Tinkoff) Cancel order has finished with error: " <> show eRes
        