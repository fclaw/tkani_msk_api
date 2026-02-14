{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Workers.OrderDeliveryScheduler (runOrderDeliveryScheduler) where


import Katip
import Data.Time -- for time-of-day checking
import Data.Text (Text)
import Data.Functor ((<&>))
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

  let pickupStartHour = 9 -- Default to 7:00

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
        let items = generateArrivalNotification today xs
        if not (T.null items) then do
          let body = HM.fromList [("items", items)]
          message <- fmap escapeMarkdownV2 $ render ($currentModule) body
          eResp <- sendOrEditTelegramMessage mempty message MAIN Nothing Nothing Nothing
          extractFromEither eResp $ \MessageIdResponse {..} ->
            void $ insertTelegramOrderDeliveryPost (fromIntegral message_id) pool
         else pure () 
    $(logTM) InfoS "Order delivery scheduler has finished."

  -- F. If it is a new day (after midnight), reset the lock.
  liftIO $ atomically $ do
    lastRun <- readTVar lastRunVar
    when (lastRun /= Just today) $
      writeTVar lastRunVar Nothing


-- Map each order to a multi-line formatted block
generateArrivalNotification :: Day -> [OrderDeliveryItem] -> T.Text
generateArrivalNotification today orders =
  let
     -- Filter the orders first: only keep those that expire soon.
     expiringOrders = filter (isExpiringSoon today) orders
     -- Format only the expiring orders.
     formattedBlocks = zipWith (formatBlock today) [1..] expiringOrders
  in
     -- Only join blocks if there are any, otherwise return empty text.
     -- (This implicitly handles the case where no items are expiring.)
     T.intercalate "\n---\n" formattedBlocks
  where
    -- Define what "expiring soon" means (e.g., 5 days or less).
    -- This function must align with the expiry logic inside formatBlock.
    isExpiringSoon :: Day -> OrderDeliveryItem -> Bool
    isExpiringSoon today odi =
      case odiKeepFreeUntil odi of
        Nothing -> False -- No expiry date, so not 'expiring soon'
        Just keepUntilDate ->
          let daysLeft = diffDays (utctDay keepUntilDate) today
          in daysLeft <= 1 -- True if 5 days or less remaining


-- Helper to format a single order block with all its details.
formatBlock :: Day -> Int -> OrderDeliveryItem -> Text
formatBlock today index OrderDeliveryItem {..} =
  let
     -- Essential lines are always present.
     baseLines =
      [ T.pack (show index) <> " - заказ: `" <> odiId <> "`"
      , "СДЭК: `" <> odiTrack <> "`"
      ]

     -- Build the expiry line only if the date is present and needs special formatting.
     expiryLine :: [Text]
     expiryLine =
       case odiKeepFreeUntil of
         Nothing -> [] -- If no expiry date, add nothing.
         Just keepUntilDate ->
          let
             daysLeft = diffDays (utctDay keepUntilDate) today
             dateString = T.pack (formatTime defaultTimeLocale "%d %B %Y" keepUntilDate)
             -- Determine if a warning is needed.
             postfix = if daysLeft <= 1 then " ⚠️" else mempty
          in
             [ "Хранение до: *" <> dateString <> "*" <> postfix ] -- This is a single line, inside a list.

  -- Combine all parts and join them.
  in T.intercalate "\n" (baseLines ++ expiryLine) -- Only non-empty lists will be concatenated.