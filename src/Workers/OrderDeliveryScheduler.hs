{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Workers.OrderDeliveryScheduler (runOrderDeliveryScheduler) where


import Katip
import Data.Time -- for time-of-day checking
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Time.LocalTime (localTimeOfDay, utcToLocalTime, zonedTimeToUTC, TimeZone(..))
import Control.Monad.Reader.Class (ask)
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar, atomically)
import Data.Maybe (fromMaybe)
import Control.Monad (when, void)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time (Day, diffDays, utctDay)
import System.IO.Unsafe (unsafePerformIO) -- For getting 'today' in a pure context (see note)


import Text (tshow)
import Utils.Time (utcToMoscowZonedTime)
import TH.Location (currentModule)
import App (AppM, _appDBPool, extractFromEither, ChatKey (MAIN), render)
import Infrastructure.Database (fetchOrderDeliveryItem, insertTelegramOrderDeliveryPost, OrderDeliveryItem (..))
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, deleteMessage, MessageIdResponse (..))
import Utils.Telegram.Markdown (escapeMarkdownV2)


-- | The main loop for the scheduled SDEK pickup job.
-- | A state for the scheduler to remember the date of its last successful run.
type LastRunDay = TVar (Maybe Day)

-- A simple helper to get today's date.
-- Using unsafePerformIO here is a common, pragmatic approach for a 'today'
-- value in a function that is conceptually pure for a given run.
today :: Day
today = utctDay (unsafePerformIO getCurrentTime)
{-# NOINLINE today #-}

-- | The main scheduler function. It takes the TVar as an argument.
runOrderDeliveryScheduler :: TVar (Maybe Day) -> AppM ()
runOrderDeliveryScheduler lastRunVar = do
  $(logTM) InfoS "Order Delivery Scheduler thread started."
  -- A. Get current time info
  let msk = TimeZone (3 * 60) False "MSK"
  now <- liftIO getZonedTime
  let mskLocalTime = utcToLocalTime msk (zonedTimeToUTC now)
  let (TimeOfDay hour _ _) = localTimeOfDay mskLocalTime
  let today = localDay mskLocalTime

  let pickupStartHour = 8 -- Default to 7:00

  -- C. Check if we need to run the job
  let isRightTime = hour == pickupStartHour

  -- D. Atomically check the lock
  shouldRun <- liftIO $ atomically $ do
    lastRun <- readTVar lastRunVar
    -- Condition: We should run if it's the right time and we haven't already run today.
    if isRightTime &&
       lastRun /= Just today
    then do
      -- If we decide to run, we immediately "take the lock"
      -- by writing today's date into the TVar.
      writeTVar lastRunVar (Just today)
      return True
    else return False
        
  -- E. Execute the job if the check passed
  when shouldRun $ do
    $(logTM) InfoS "Order delivery time has come..."
    pool <- fmap _appDBPool ask
    eDbRes <- fetchOrderDeliveryItem today pool
    extractFromEither eDbRes $ \(mYesterdayPostId, xs) -> do 
      for_ mYesterdayPostId $ ((`deleteMessage` MAIN) . fromIntegral)
      when(length xs > 0) $ do
        let items = generateArrivalNotification xs
        let body = HM.fromList [("items", items)]
        message <- fmap escapeMarkdownV2 $ render ($currentModule) body
        eResp <- sendOrEditTelegramMessage mempty message MAIN Nothing Nothing Nothing
        extractFromEither eResp $ \MessageIdResponse {..} ->
          void $ insertTelegramOrderDeliveryPost (fromIntegral message_id) pool
    $(logTM) InfoS "Order delivery scheduler has finished."

  -- F. If it is a new day (after midnight), reset the lock.
  liftIO $ atomically $ do
    lastRun <- readTVar lastRunVar
    when (lastRun /= Just today) $
      writeTVar lastRunVar Nothing


-- Map each order to a multi-line formatted block
generateArrivalNotification :: [OrderDeliveryItem] -> T.Text
generateArrivalNotification orders = T.intercalate "\n" $ zipWith formatBlock [1..] orders

-- Helper to format a single order block with all its details.
formatBlock :: Int -> OrderDeliveryItem -> Text
formatBlock index OrderDeliveryItem {..} =
  let
    -- Base components of the message
    line1 = T.pack (show index) <> " - заказ: `" <> odiId <> "`"
    line2 = ", СДЭК: `" <> odiTrack <> "`"
    -- --- EXPIRY LOGIC (Simplified) ---
    expiryLine = 
      case odiKeepFreeUntil of
        -- If there's no 'keep until' date, return an empty string for this line.
        Nothing -> mempty
        
        -- If there is a date...
        Just keepUntilDate ->
          let
             -- Calculate days remaining (can be negative if overdue).
             daysLeft = diffDays (utctDay keepUntilDate) today

             -- Determine the postfix: a warning emoji if 1 day or less is left.
             postfix = if daysLeft <= 1 then " ⚠️" else mempty
                
             -- Format the date into a human-readable string.
             dateString = T.pack (formatTime defaultTimeLocale "%d %B %Y" keepUntilDate)
          in
             -- Assemble the final line.
             ", Хранение до: *" <> dateString <> "*" <> postfix

  -- Combine the lines into the final multi-line block.
  -- The filter removes the expiry line completely if it's empty.
  in T.unlines $ filter (not . T.null) [line1, line2, expiryLine]