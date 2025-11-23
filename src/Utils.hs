{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils (withRetry) where


-- Imports needed for MonadBaseControl style handling
import Control.Exception.Lifted (try, throwIO, SomeException)
import Control.Concurrent (threadDelay)
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Katip
import Data.Text (pack, Text)


showt :: Show a => a -> Text
showt = pack . show

-- | Retry helper for MonadBaseControl stacks
withRetry :: forall m a . (MonadBaseControl IO m, KatipContext m) => Int -> m a -> m a
withRetry attempts action = go 1
  where
    go attempt = do
        -- 1. Try the action
        -- explicit type annotation ensures we catch ALL standard exceptions
        result <- try action :: m (Either SomeException a)

        case result of
            Right val -> return val
            
            Left err -> do
                if attempt >= attempts
                then do
                       -- 1. FINAL FAILURE (Error Level)
                       -- We explicitly state that the limit was hit and show the final error.
                      $(logTM) ErrorS $ ls $
                         "Retry limit reached (" <> showt attempts <> " attempts). " <>
                         "Operation failed permanently. Last exception: " <> showt err 
                      throwIO err 
                else do
                       -- 2. INTERMEDIATE FAILURE (Warning Level)
                       -- We state which attempt failed, that we are retrying, and what the error was.
                      $(logTM) WarningS $ ls $
                        "Attempt " <> showt attempt <> "/" <> showt attempts <> 
                        " failed. Retrying in 2s... Exception: " <> showt err
                    
                      -- Wait 2 seconds (2,000,000 microseconds)
                      -- 'liftBase' is the MonadBaseControl equivalent of 'liftIO'
                      liftBase $ threadDelay 2000000 
                    
                      go (attempt + 1)