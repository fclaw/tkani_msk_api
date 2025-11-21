{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Workers.TinkoffPaymentStatusPoller (paymentStatusPoller) where

import Katip
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM
import Control.Monad.State.Class (get)
import Control.Monad.Reader.Class (ask)
import Control.Concurrent.Async.Lifted (async)
import Data.Text (Text, pack)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (UTCTime, diffUTCTime, NominalDiffTime)
import System.Timeout.Lifted (timeout)
import Data.Function (fix)
import Data.Maybe (isNothing)
import Data.Either (isLeft)
import qualified Data.HashMap.Strict as HM
import Data.Time (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import Data.Foldable (for_)

import App (AppM, runAppM, _tinkoffPaymentChan, _appDBPool, currentTime, ChatKey (CONCIERGE), render)
import Infrastructure.Services.Tinkoff (PaymentDetails (..), checkTinkoffPaymentStatus, Status (..))
import Infrastructure.Database (getChatDetails)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import TH.Location (currentModule)
import Domain.Inventory (adjustInventoryForOrder, InventoryResult (..))


-- Configuration constants (in microseconds)
delayFast   =  4 * 1000000 -- 4 seconds
delayMedium = 12 * 1000000 -- 12 seconds
delaySlow   = 45 * 1000000 -- 45 seconds

readTVar = liftIO . STM.atomically . STM.readTVar
readTChan = liftIO . STM.atomically . STM.readTChan

paymentStatusPoller :: AppM ()
paymentStatusPoller = do
  -- Run the core logic within our application's monad to get access to the DB, logger, etc.
  $(logTM) InfoS "Polling for Tinkoff payment statuses..."
  stvar <- get
  st <- readTVar stvar
  -- 1. Read from channel (Blocks until a new order arrives)
  -- 2. Spawn a GREEN THREAD immediately (async).
  forever $ readTChan (_tinkoffPaymentChan st) >>= (void . async . workerLogic)

workerLogic :: PaymentDetails -> AppM ()
workerLogic PaymentDetails {..} = do
  startTime <- currentTime

  -- 3. Set the hard limit (20 minutes = 1200 seconds)
  -- Using timeout to kill this specific thread if it runs too long
  let twentyMinutesMicros = 20 * 60 * 1000000
    
  mResult <- timeout twentyMinutesMicros $ fix $ \loop -> do
    -- A. Check Time Elapsed
    now <- currentTime
    let elapsed = diffUTCTime now startTime
    -- B. Call Tinkoff API
    eStatus <- checkTinkoffPaymentStatus paymentId
    if isLeft eStatus 
    then return ()
    else
      let Right status = eStatus in
      case status of
        ------------------------------------------------------------
        -- 1. SUCCESS STATES (Stop Polling)
        ------------------------------------------------------------
        s | s `elem` [Confirmed, Authorized] -> do
          $(logTM) InfoS $ ls $ "Order " <>  orderId <> " PAID (" <> pack (show s) <> ")."    
          -- Update Telegram: Send "Green" template
          currentTime >>= finalizeTelegram orderId "Success"
          eInventoryResult <- adjustInventoryForOrder orderId
          for_ eInventoryResult $ \case
            StockOK -> return ()
            FabricSoldOut _ -> return ()
          when(isLeft eInventoryResult) $ $(logTM) ErrorS $ ls $ "error: " <> show eInventoryResult
          -- EXIT LOOP
        ------------------------------------------------------------
        -- 2. HARD FAILURE (Card Declined / Reversed) (Stop Polling)
        ------------------------------------------------------------
        s | s `elem` [Rejected, Reversed] -> do
          $(logTM) WarningS $ ls $ "Order " <> orderId <> " REJECTED (" <> pack (show s) <> ")."
          -- Update Telegram: Send "Red" template
          currentTime >>= finalizeTelegram orderId "Declined"
          -- EXIT LOOP
        ------------------------------------------------------------
        -- 3. EXPIRED / TIMEOUT (Bank side) (Stop Polling)
        ------------------------------------------------------------
        DeadlineExpired -> do
          $(logTM) InfoS $ ls $ "Order " <> orderId <> " EXPIRED (Tinkoff)."
          -- Update Telegram: Send "Yellow/Timeout" template
          currentTime >>= finalizeTelegram orderId "Timeout"
          -- EXIT LOOP
        ------------------------------------------------------------
        -- 4. CANCELED (Merchant or User aborted) (Stop Polling)
        ------------------------------------------------------------
        Canceled -> do
          $(logTM) InfoS $ ls $ "Order " <> orderId <> " CANCELED."
          -- Reuse Timeout or Failed template, or make a specific "Gray" one
          currentTime >>= finalizeTelegram orderId "Declined"
          -- EXIT LOOP
        ------------------------------------------------------------
        -- 5. CONTINUE POLLING (New, Processing, Unknown)
        ------------------------------------------------------------
        other -> do
          case other of
            Unknown t -> $(logTM) WarningS $ ls $ "Unknown status for " <> orderId <> ": " <> t
            _         -> return ()
          let sleepTime = getAdaptiveDelay elapsed
          liftIO (threadDelay sleepTime) >> loop -- RECURSIVE CALL

  -- 4. Handle Timeout Case (if result is Nothing)
  when(isNothing mResult) $ do 
    $(logTM) WarningS $ ls $ "Order " <> orderId <> " TIMED OUT."
    currentTime >>= finalizeTelegram orderId "Timeout"

-- Helper for strategy
getAdaptiveDelay :: NominalDiffTime -> Int
getAdaptiveDelay elapsed 
  | elapsed > 0 && 
    elapsed < 180      = delayFast   -- 0 to 3 min (180s)
  | elapsed >= 180 && 
    elapsed < 360      = delayMedium -- 3 to 6 min (360s)
  | otherwise          = delaySlow   -- 6 to 20 min


finalizeTelegram :: Text -> Text ->  UTCTime -> AppM ()
finalizeTelegram orderId suffix tm = do
  tz <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime tz tm
  let timeStr = pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" localTime
  let templateData = HM.fromList [("orderId", orderId), ("timestamp", timeStr)]
  message <- render ($currentModule <> "." <> suffix) templateData

  cfg <- ask
  let pool = _appDBPool cfg
  eMessageId <- liftIO $ getChatDetails orderId pool
  for_ eMessageId $ \messageId ->
    void $ sendOrEditTelegramMessage ("tinkoff poller: " <> orderId) message CONCIERGE messageId
  when(isLeft eMessageId) $ $(logTM) ErrorS $ ls $ "error while fetching chat details " <> pack (show eMessageId)