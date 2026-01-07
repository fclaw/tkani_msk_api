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


import Text (tshow)
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

  let pickupStartHour = 7 -- Default to 7:00

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
    extractFromEither eDbRes $ \(mYesterdayPostId, items) -> do 
      for_ mYesterdayPostId $ ((`deleteMessage` MAIN) . fromIntegral)
      let list = makeBody (zip [1..] items) mempty
      let body = HM.fromList [("items", list)]
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

makeBody [] body = body
makeBody ((idx, OrderDeliveryItem {..}) : rest) oldBody = makeBody rest newBody
  where newBody | T.empty oldBody = tshow idx <> " - " <> "заказ: " <> odiId <> ", СДЭК: " <> odiTrack 
                | otherwise = oldBody <> "\n" <> tshow idx <> " - " <> "заказ: " <> odiId <> ", СДЭК: " <> odiTrack