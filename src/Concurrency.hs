{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concurrency (pooledForConcurrentlyN, runJobWithCleanup) where

import Katip
import Control.Concurrent.Async.Lifted (mapConcurrently)
import Control.Monad (forM)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar, atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Lifted (finally, try, SomeException)

import App (AppM)


-- | Helper to split list into chunks of N
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunkList n rest

-- | maps an action over a list using at most N concurrent threads.
-- Valid types: IO, ReaderT config IO, AppM, etc.
pooledForConcurrentlyN 
  :: (MonadBaseControl IO m, MonadIO m)
  => Int          -- ^ Max threads
  -> [a]          -- ^ Input list
  -> (a -> m b)   -- ^ Action to apply
  -> m [b]        -- ^ Ordered list of results
pooledForConcurrentlyN n _ _ | n <= 0 = error "pooledForConcurrentlyN: thread limit must be > 0"
pooledForConcurrentlyN n xs action = do
    -- 1. Split input into batches
    let batches = chunkList n xs
    -- 2. Run batches Sequentially (Monadic bind)
    --    Inside each batch, run Concurrently (Async)
    resultsOfBatches <- forM batches $ mapConcurrently action
    -- 3. Flatten the results
    return (concat resultsOfBatches)

-- | A simple flag to track the outcome of the worker.
data WorkerStatus = InProgress | Succeeded | Failed
  deriving (Eq)

-- This is the code you would put inside your 'async' block
runJobWithCleanup :: AppM () -> AppM ()
runJobWithCleanup job = do
    -- 1. Create a status flag, initialized to 'InProgress'.
    statusVar <- liftIO $ newTVarIO InProgress

    -- 2. Define the main action and the cleanup action.
    let mainAction = do
            -- Use 'try' to catch synchronous exceptions *within* the job.
            eResult <- try job
            
            -- If it completes (even with a logical error), it's a 'success' in terms of execution.
            -- A crash (async exception) will prevent this line from being reached.
            liftIO $ atomically $ writeTVar statusVar Succeeded
            
            -- Handle the logical result
            case eResult of
                Left (ex :: SomeException) -> do
                    $(logTM) ErrorS $ "Job failed with synchronous exception: " <> ls (show ex)
                    liftIO $ atomically $ writeTVar statusVar Failed -- <-- SET FAILED
                Right _ -> do
                    $(logTM) InfoS "Job completed successfully."
                    liftIO $ atomically $ writeTVar statusVar Succeeded -- <-- SET SUCCEEDED

    let cleanupAction = do
            -- 3. The cleanup block runs no matter what.
            finalStatus <- liftIO $ atomically $ readTVar statusVar
            
            -- 4. Check the flag to determine what happened.
            if finalStatus == Succeeded
            then
                -- The job finished normally.
                $(logTM) DebugS "Cleanup: Job finished normally."
            else
                -- The job was killed by an asynchronous exception (e.g., timeout)
                -- or crashed before it could set the flag to Succeeded.
                $(logTM) WarningS $ "Cleanup: Job was terminated unexpectedly (e.g., by timeout or crash)."
                -- You could add logic here to put the job back on a queue for a retry.

    -- 5. Run the main action with the cleanup guaranteed by 'finally'.
    mainAction `finally` cleanupAction