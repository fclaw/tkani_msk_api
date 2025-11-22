{-# LANGUAGE FlexibleContexts #-}

module Concurrency (pooledForConcurrentlyN) where

import Control.Concurrent.Async.Lifted (forConcurrently)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)

-- | maps an action over a list using at most N concurrent threads.
-- Valid types: IO, ReaderT config IO, AppM, etc.
pooledForConcurrentlyN 
  :: (MonadBaseControl IO m, MonadIO m)
  => Int          -- ^ Max threads
  -> [a]          -- ^ Input list
  -> (a -> m b)   -- ^ Action to apply
  -> m [b]        -- ^ Ordered list of results
pooledForConcurrentlyN n _ _ | n <= 0 = error "pooledForConcurrentlyN: thread limit must be > 0"
pooledForConcurrentlyN n xs action = runJobs xs
  where
    -- Helper: Take 'n' items, run them, append results, recurse.
    runJobs [] = return []
    runJobs input = do
      let (chunk, rest) = splitAt n input
      -- 1. Run this batch (concurrency is limited by chunk size 'n')
      chunkResults <- forConcurrently chunk action
      -- 2. Recurse for the rest
      restResults <- runJobs rest
      -- 3. Combine (Results are strictly ordered)
      return (chunkResults ++ restResults)