{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Application.Cart (runCartsCleaner) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Katip
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import Control.Monad.Reader.Class (ask)

import App (AppM, _appDBPool)
import Infrastructure.Database (clearOldCarts)


runCartsCleaner :: AppM ()
runCartsCleaner = do
  $(logTM) InfoS "Carts Cleaner starts..." 
  forever $ do
    -- Run the core logic within our application's monad to get access to the DB, logger, etc.
    pool <- fmap _appDBPool ask
    eRes <- liftIO $ clearOldCarts pool
    when(isLeft eRes) $ $(logTM) ErrorS $ ls $ "runCartsCleaner error: " <> show eRes
    liftIO $ threadDelay (60 * 1000000)