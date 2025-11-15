{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Utils.OrderId (generateOrderId) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime, defaultTimeLocale, formatTime)
import           System.Random (randomRIO) -- The core function for randomness
import           Control.Monad (replicateM) -- A handy utility for monadic repetition


-- | A constant string containing only unambiguous characters.
--   This avoids confusion between 0/O, 1/l/I, etc.
unambiguousChars :: String
unambiguousChars = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"

-- | Generates a random alphanumeric string of a given length from the
--   unambiguous character set.
generateRandomPart :: Int -> IO String
generateRandomPart len = do
  -- 1. Get the upper bound for our random index
  let maxIndex = length unambiguousChars - 1
  -- 2. Generate 'len' random indices within that bound
  --    replicateM is perfect: it runs an IO action 'n' times and collects the results.
  indices <- replicateM len (randomRIO (0, maxIndex))
  -- 3. Map each random index to a character from our set
  pure $ map (unambiguousChars !!) indices


-- | Generates a unique, human-friendly, and chronologically sortable order ID.
--   Format: ORD-YYYYMMDD-XXXXXX
--   This function has NO external dependencies beyond 'base', 'time', and 'text'.
generateOrderId :: IO Text
generateOrderId = do
  -- Part 1: The static prefix
  let prefix = "ORD-"
  -- Part 2: The timestamp (YYYYMMDD)
  timeStampPart <- T.pack . formatTime defaultTimeLocale "%Y%m%d-" <$> getCurrentTime
  -- Part 3: A 6-character random string using our custom helper
  randomChars <- generateRandomPart 6
  let randomPart = T.pack randomChars
  -- Combine them all!
  pure $ T.concat [prefix, timeStampPart, randomPart]