module Utils.OrderId (generateOrderId) where

-- import           Data.Text (Text)
-- import qualified Data.Text as T
-- import           Data.Time (getCurrentTime, UTCTime, defaultTimeLocale, formatTime)
-- import           Text.StringRandom (randomString, unambiguous)

-- | Generates a unique, human-friendly, and chronologically sortable order ID.
--   Format: ORD-YYYYMMDD-XXXXXX (where X is a random, unambiguous character)
generateOrderId :: IO ()
generateOrderId = undefined
  -- Part 1: The static prefix
--   let prefix = "ORD-"
--   -- Part 2: The timestamp (YYYYMMDD)
--   timeStampPart <- T.pack . formatTime defaultTimeLocale "%Y%m%d-" <$> getCurrentTime
--   -- Part 3: A 6-character random string using only unambiguous characters.
--   -- This avoids confusion between 0/O, 1/l/I, etc.
--   -- The character set for 'unambiguous' is: ABCDEFGHJKLMNPQRSTUVWXYZ23456789
--   randomPart <- randomString unambiguous 6
--   -- Combine them all!
--   pure $ T.concat [prefix, timeStampPart, randomPart]

-- -- Example Usage (in a main file for testing):
-- -- main :: IO ()
-- -- main = do
-- --   orderId <- generateOrderId
-- --   TIO.putStrLn $ "Generated Order ID: " <> orderId
-- --   -- Example Output: Generated Order ID: ORD-20231114-A4B7X2