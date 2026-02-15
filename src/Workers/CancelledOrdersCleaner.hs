{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Workers.CancelledOrdersCleaner (runCancelledOrdersCleaner) where


import Katip
import Control.Monad (void, when)
import Data.Foldable (for_)
import Data.Either (isLeft)
import Control.Monad.Reader.Class (ask)

import Text (tshow)
import App (AppM, ChatKey (..), _appDBPool)
import Infrastructure.Services.Telegram (deleteMessage)
import Infrastructure.Database (fetchCancelledOrders, markedCancelledOrders, CancelledOrders (..))

runCancelledOrdersCleaner :: AppM ()
runCancelledOrdersCleaner = do
  $(logTM) InfoS "CancelledOrdersCleaner started."
  cfg <- ask
  let pool = _appDBPool cfg
  eDbRes <- fetchCancelledOrders pool
  for_ eDbRes $ \orders -> do
    -- liftIO $ print orders
    for_ orders $ \CancelledOrders {..} ->
      when(coMessageId > 0) $ 
        void $ deleteMessage coMessageId coChannel
    markedCancelledOrders [(coOrderId o, coChannel o) | o <- orders] pool
  when (isLeft eDbRes) $ $(logTM) ErrorS $ "CancelledOrdersCleaner db failed " <> ls (tshow eDbRes)