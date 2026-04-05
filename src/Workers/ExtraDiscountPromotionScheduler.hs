{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Workers.ExtraDiscountPromotionScheduler 
       (runExtraDiscountPromotionScheduler) where


import Katip
import Data.Time
import Data.Int (Int64, Int32)
import Control.Monad (when, void)
import System.Random (randomRIO)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar (diffDays, addGregorianMonthsClip)
import Control.Monad.Reader.Class (ask)

import Text (tshow)
import TH.Location (currentModule)
import App (AppM, _appDBPool, currentTime, ChatKey (MAIN), render)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (deleteMessage, sendOrEditTelegramMessage, MessageIdResponse (..))
import Infrastructure.Database (fetchExtraDiscountDetails, updateExtraDiscountPromotions, setMessageIdExtraDiscountPromotions, insertStartPromotion)

-- | Extracts the Local Moscow Hour and the exact Date (Day)
getMoscowStats :: AppM (Int, Day)
getMoscowStats = do
  -- 1. Get global time
  now <- currentTime
    
  -- 2. Define Moscow offset (UTC+3)
  let moscowZone = hoursToTimeZone 3
  let localNow   = utcToLocalTime moscowZone now
    
   -- 3. Return a pair of (Hour [0..23], Exact Date)
  return (todHour (localTimeOfDay localNow), localDay localNow)


runExtraDiscountPromotionScheduler :: AppM ()
runExtraDiscountPromotionScheduler = do
  $(logTM) InfoS "ExtraDiscountPromotionScheduler started."

  -- if midnight, check if there are any promotions that should be activated or deactivated based on the current date and time.
  -- This would involve querying the database for promotions, 
  -- checking their start and end times, and updating their status accordingly. 
  -- You might also want to send notifications to users about new promotions or expiring
  -- A extract hour and day from current time 
  (hour, day) <- getMoscowStats
  when(hour == 0) $ do
    $(logTM) InfoS 
      "It's midnight in Moscow. \
      \ Checking promotions to \
      \ activate/deactivate."
    cfg <- ask
    let pool = _appDBPool cfg
    eDbRes <- fetchExtraDiscountDetails pool
    case eDbRes of
      Left err -> 
        $(logTM) ErrorS $ ls $ 
        "Failed to fetch promotion details: " <> err
      Right (Just details@(ident, promDay, isEnabled, extraDiscount, messageId)) -> do
        $(logTM) InfoS $ ls $ "Promotion details: " <> tshow details
        -- Here you would add logic to check if the promotion should be activated or deactivated based on the current date and time.
        -- For example:
        -- when (shouldActivate details day) $ activatePromotion details
        -- when (shouldDeactivate details day) $ deactivatePromotion details 
        when(day == promDay && not isEnabled) $ do
          $(logTM) InfoS $ ls $ "Activating promotion for day: " <> tshow promDay
          activatePromotion ident promDay extraDiscount 
        when(diffDays day promDay == 1 && isEnabled) $ do
          $(logTM) InfoS $ ls $ "Deactivating promotion for day: " <> tshow promDay
          deactivatePromotion ident promDay messageId
      Right Nothing -> do 
        $(logTM) InfoS 
          "No active promotions found in the database. \
          \ Fall back to default behavior (if any)."
        let promDay = read @Day "2026-04-05"
        let extraDiscount = 0.20
        let extraDiscountInt = round (extraDiscount * 100) :: Int32
        eIdent <- insertStartPromotion promDay extraDiscount pool
        when(day == promDay) $ for_ eIdent $ \ident -> activatePromotion ident promDay extraDiscountInt -- default 20% discount


activatePromotion :: Int64 -> Day -> Int32 -> AppM ()
activatePromotion ident promDay extraDiscount = do
  let templateData = HM.fromList [("extraDiscount", tshow extraDiscount)]
  msg <- fmap escapeMarkdownV2 $ render $currentModule templateData
  eTelRes <- sendOrEditTelegramMessage mempty msg MAIN Nothing Nothing Nothing
  for_ eTelRes $ \MessageIdResponse {..} -> do
    cfg <- ask
    let pool = _appDBPool cfg
    $(logTM) InfoS $ ls $ "Promotion activated with message ID: " <> tshow message_id
    setMessageIdExtraDiscountPromotions ident message_id pool

-- | The Resulting Promo Configuration
data PromoPlan = PromoPlan
  { nextDate     :: Day
  , nextDiscount :: Double -- range 0.1 to 0.2
  } deriving (Show)

-- | The core planning function for your Janitor
planNextPromotion :: Day -> IO PromoPlan
planNextPromotion today = do
  -- 1. Date Logic: Jump +1 month and add a random 1-7 day offset
  let baseDate = addGregorianMonthsClip 1 today
  offset <- randomRIO (1, 7)
  let finalDate = addDays offset baseDate

  -- 2. Discount Logic: Random value between 10% and 20%
  -- Using Int for whole percentages makes marketing messages look better
  rawPercent <- randomRIO @Int (5, 15)
  let finalDiscount = fromIntegral rawPercent / 100.0

  return $ PromoPlan finalDate finalDiscount


deactivatePromotion :: Int64 -> Day -> Maybe Int64 -> AppM ()
deactivatePromotion ident promDay messageId = do
  cfg <- ask
  let pool = _appDBPool cfg
  PromoPlan {..} <- liftIO $ planNextPromotion promDay
  updateExtraDiscountPromotions ident nextDate nextDiscount pool
  for_ messageId $ flip deleteMessage MAIN