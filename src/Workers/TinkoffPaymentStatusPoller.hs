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
import Data.Either (isLeft, fromLeft)
import qualified Data.HashMap.Strict as HM
import Data.Time (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import Data.Foldable (for_)
import Control.Exception (SomeException)
import Control.Exception.Lifted (catch)

import App (AppM, runAppM, _tinkoffPaymentChan, _appDBPool, currentTime, ChatKey (..), render, _tinkoffCred, tinkoffTerminalKey, tinkoffSecret)
import  API.Types (OrderStatus (Cancelled))
import Infrastructure.Services.Tinkoff (checkTinkoffPaymentStatus)
import Infrastructure.Database (getChatDetails, fetchPendingPayments, updatePaymentStatus)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, deleteMessage)
import TH.Location (currentModule)
import Domain.Inventory (adjustInventoryForOrder, InventoryResult (..), Template (..))
import Infrastructure.Services.Tinkoff.Types.GetState
import Infrastructure.Services.Tinkoff.Security (generateGetStateToken, GetStateToken(..))
import Utils.Telegram.Markdown (escapeMarkdownV2)


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
  $(logTM) InfoS "Tinkoff payment poller dispatcher started."
  -- Get the channel from the application state.
  st <- readTVar stvar
  let chan = _tinkoffPaymentChan st
    
  -- fetch pending payments and enqueue them
  pool <- fmap _appDBPool ask
  ePendingPayments <- liftIO $ fetchPendingPayments pool
  for_ ePendingPayments $ \xs -> do
    tinkoffCred <- fmap _tinkoffCred ask
    for_ xs $ \(orderId, paymentId) -> do
      let getStateRequest = 
            GetStateRequest
            { gsrqPaymentId = paymentId
            , gsrqToken = 
                generateGetStateToken $
                  GetStateToken
                   paymentId
                   (tinkoffTerminalKey tinkoffCred)
                   (tinkoffSecret tinkoffCred)
            , gsrqTerminalKey = tinkoffTerminalKey tinkoffCred
            , gsrqIP = Nothing
            }
      liftIO $ STM.atomically $ STM.writeTChan chan (orderId, getStateRequest)
      $(logTM) InfoS $ ls $ "enqueued pending payment for Order " <> orderId

  when(isLeft ePendingPayments) $ do
    $(logTM) ErrorS $ ls $ "error while fetching pending payments: " <> pack (show (fromLeft undefined ePendingPayments))
    error $ "while fetching pending payments: " <> (show (fromLeft undefined ePendingPayments))    

  -- Loop forever, dispatching a new worker for each job that arrives.
  forever $ do
    job <- readTChan chan
    $(logTM) InfoS $ ls $ "Dispatching worker for Order " <> fst job
    void $ async $ workerLogic job

-- | The logic for a single worker thread. It polls one payment until a final status is reached.
workerLogic :: (Text, GetStateRequest) -> AppM ()
workerLogic job@(orderId, _) =
    -- Wrap the entire worker in an exception handler to prevent silent crashes.
    catch (processJob job) (handleWorkerError job)

-- | Global exception handler for the worker thread.
handleWorkerError :: (Text, GetStateRequest) -> SomeException -> AppM ()
handleWorkerError (orderId, _) e = do
    $(logTM) ErrorS $ ls $ "CRITICAL: Worker for Order " <> orderId <> " crashed. Exception: " <> pack (show e)
    currentTime >>= finalizeTelegram orderId "Error" -- Notify user of a system error

processJob :: (Text, GetStateRequest) -> AppM ()
processJob (orderId, getStateReq) = do
  -- 1. Log start
  $(logTM) InfoS $ ls $ "Worker started for Order " <> orderId
  startTime <- currentTime
  -- 3. Set the hard limit (20 minutes = 1200 seconds)
  -- Using timeout to kill this specific thread if it runs too long
  let twentyMinutesMicros = 20 * 60 * 1000000
    
  mResult <- timeout twentyMinutesMicros $ fix $ \loop -> do
    -- A. Check Time Elapsed
    now <- currentTime
    let elapsed = diffUTCTime now startTime
    -- B. Call Tinkoff API
    eStatus <- checkTinkoffPaymentStatus getStateReq
    if isLeft eStatus 
    then do
      let Left apiError = eStatus
      $(logTM) ErrorS $ ls $ "API error while checking payment status for Order " <> orderId <> ": " <> pack (show apiError)
    else
      let Right status = eStatus in
      case status of
        ------------------------------------------------------------
        -- 1. SUCCESS STATES (Stop Polling)
        ------------------------------------------------------------
        s | s `elem` [CONFIRMED, AUTHORIZED] -> do
          $(logTM) InfoS $ ls $ "Order " <>  orderId <> " PAID (" <> pack (show s) <> ")."    
          -- Update Telegram: Send "Green" template
          currentTime >>= finalizeTelegram orderId "Success"
          eInventoryResult <- adjustInventoryForOrder orderId
          for_ eInventoryResult $ \case
            StockOK msgId -> replyMessage msgId
            FabricSoldOutOrPrecut msgId xs -> do
              replyMessage msgId
              for_ xs $ \case
                RollBranch maybeMsgId renderMessage -> do
                  msg <- renderMessage
                  notifyMessage $ escapeMarkdownV2 msg
                  for_ maybeMsgId $ (`deleteMessage` WAREHOUSE)
                PrecutBranch msgId -> void $ deleteMessage msgId WAREHOUSE
          when(isLeft eInventoryResult) $ $(logTM) ErrorS $ ls $ "error: " <> show (fromLeft undefined eInventoryResult)
          -- EXIT LOOP
        ------------------------------------------------------------
        -- 2. HARD FAILURE (Card Declined / Reversed) (Stop Polling)
        ------------------------------------------------------------
        s | s `elem` [REJECTED, REVERSED] -> do
          $(logTM) WarningS $ ls $ "Order " <> orderId <> " REJECTED (" <> pack (show s) <> ")."
          -- Update Telegram: Send "Red" template
          currentTime >>= finalizeTelegram orderId "Declined"
          pool <- fmap _appDBPool ask
          eRes <- liftIO $ updatePaymentStatus orderId REJECTED Cancelled pool
          when(isLeft eRes) $ 
            $(logTM) ErrorS $ ls $ 
              "error while updating payment status for Order " <> 
              orderId <> ": " <> 
              pack (show (fromLeft undefined eRes))
          -- EXIT LOOP
        ------------------------------------------------------------
        -- 3. EXPIRED / TIMEOUT (Bank side) (Stop Polling)
        ------------------------------------------------------------
        DEADLINE_EXPIRED -> do
          $(logTM) InfoS $ ls $ "Order " <> orderId <> " EXPIRED (Tinkoff)."
          -- Update Telegram: Send "Yellow/Timeout" template
          currentTime >>= finalizeTelegram orderId "Timeout"
          pool <- fmap _appDBPool ask
          eRes <- liftIO $ updatePaymentStatus orderId CANCELLED Cancelled pool
          when(isLeft eRes) $ 
            $(logTM) ErrorS $ ls $ 
              "error while updating payment status for Order " <> 
              orderId <> ": " <> 
              pack (show (fromLeft undefined eRes))
          -- EXIT LOOP
        ------------------------------------------------------------
        -- 4. CANCELLED (Merchant or User aborted) (Stop Polling)
        ------------------------------------------------------------
        CANCELLED -> do
          $(logTM) InfoS $ ls $ "Order " <> orderId <> " CANCELLED."
          -- Reuse Timeout or Failed template, or make a specific "Gray" one
          currentTime >>= finalizeTelegram orderId "Declined"
          pool <- fmap _appDBPool ask
          eRes <- liftIO $ updatePaymentStatus orderId CANCELLED Cancelled pool
          when(isLeft eRes) $ 
            $(logTM) ErrorS $ ls $ 
              "error while updating payment status for Order " <> 
              orderId <> ": " <> 
              pack (show (fromLeft undefined eRes))
          -- EXIT LOOP
        ------------------------------------------------------------
        -- 5. CONTINUE POLLING (New, Processing, Unknown)
        ------------------------------------------------------------
        other -> do
          case other of
            UNKNOWN_STATUS t -> $(logTM) WarningS $ ls $ "Unknown status for " <> orderId <> ": " <> t
            _         -> return ()
          let sleepTime = getAdaptiveDelay elapsed
          liftIO (threadDelay sleepTime) >> loop -- RECURSIVE CALL

  -- 4. Handle Timeout Case (if result is Nothing)
  when(isNothing mResult) $ do 
    $(logTM) WarningS $ ls $ "Order " <> orderId <> " TIMED OUT."
    currentTime >>= finalizeTelegram orderId "Timeout"
    pool <- fmap _appDBPool ask
    eRes <- liftIO $ updatePaymentStatus orderId CANCELLED Cancelled pool
    when(isLeft eRes) $ 
      $(logTM) ErrorS $ ls $
        "error while updating payment status for Order " <> 
        orderId <> ": " <> 
        pack (show (fromLeft undefined eRes))

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
  message <- fmap escapeMarkdownV2 $ render ($currentModule <> "." <> suffix) templateData

  cfg <- ask
  let pool = _appDBPool cfg
  eMessageId <- liftIO $ getChatDetails orderId pool
  for_ eMessageId $ \messageId ->
    void $ sendOrEditTelegramMessage ("tinkoff poller: " <> orderId) message CONCIERGE messageId Nothing Nothing
  when(isLeft eMessageId) $ $(logTM) ErrorS $ ls $ "error while fetching chat details " <> pack (show eMessageId)


notifyMessage :: Text -> AppM ()
notifyMessage message = void $ sendOrEditTelegramMessage mempty message ORDER Nothing Nothing Nothing

replyMessage :: Int -> AppM ()
replyMessage msgId = do 
  message <- fmap escapeMarkdownV2 $ render ($currentModule <> ".Paid") mempty
  void $ sendOrEditTelegramMessage mempty message ORDER Nothing (Just msgId) Nothing

replyPrecutBought :: Int -> Text -> AppM ()
replyPrecutBought msgId message = void $ sendOrEditTelegramMessage mempty message WAREHOUSE Nothing (Just msgId) Nothing