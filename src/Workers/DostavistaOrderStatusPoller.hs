{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Workers.DostavistaOrderStatusPoller (runDostavistaOrderStatusPoller) where

import Katip
import Data.Int (Int64)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Data.Time (utctDay, UTCTime)
import Data.Foldable (for_)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (diffUTCTime)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Concurrent.Async.Lifted (async)

import App
import API.Types (OrderStatus(PickedUpByCourier))
import Text (tshow)
import TH.Location (currentModule)
import Concurrency (runJobWithCleanup)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Dostavista (getOrder, cancelOrder)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Infrastructure.Services.Dostavista.Types.Enums (DostavistaOrderStatus (..))
import Infrastructure.Services.Dostavista.Types (DostavistaOrdersResponse (..), Order (..), Courier (..))
import  Infrastructure.Database (getTodaysDostavistaOrder, setDostavistaOrderStatus, setDostavistaPickupByCourierStatus)



runDostavistaOrderStatusPoller :: AppM ()
runDostavistaOrderStatusPoller = do
   $(logTM) InfoS "DostavistaOrderStatusPoller started."
   cfg <- ask
   pool <- fmap _appDBPool ask
   tm <- currentTime
   let today = utctDay tm
   eDbRes <- getTodaysDostavistaOrder today pool
   extractFromEither eDbRes $ \maybeOrderInfo -> do
     appStateVar <- get
     appState <- readTVarIO appStateVar
     let chan = _dostavistaChan appState
     for_ maybeOrderInfo $ \(order, status, start) ->
       writeTChanIO chan $ DostavistaJob order status start
     forever $ do
      job <- readTChanIO chan
      statusVar <- liftIO $ newTVarIO $ (doJobOrderStatus job, doJobAStart job)
      void $ async $ runJobWithCleanup $ pollerLogic statusVar (doJobOrderId job)


timeout :: Int
timeout = 30 * 1000000

pollerLogic :: TVar (DostavistaOrderStatus, UTCTime) -> Int64 -> AppM ()
pollerLogic statusVar orderId = do
  (currentStatus, start) <- readTVarIO statusVar
  end <- currentTime

  let timeElapsed = round $ diffUTCTime end start

   -- if no courier is found within 4-hour window close the order
  if timeElapsed > 4 * 3600 &&
     (currentStatus == Available || 
     currentStatus == New)
  then do
    pool <- fmap _appDBPool ask
    void $ setDostavistaOrderStatus orderId Canceled pool
    $(logTM) InfoS $ "Dostavista order " <> ls (show orderId) <> " has been cancelled due to timeout."
    let msg = escapeMarkdownV2 $ "⚠️ Dostavista order " <> tshow orderId <> " has been cancelled automatically due to missing courier. timeout"
    void $ sendOrEditTelegramMessage mempty msg ORDER Nothing Nothing Nothing
  else do
    pool <- fmap _appDBPool ask
    eResp <- getOrder orderId
    case eResp of
      Left apiErr -> $(logTM) ErrorS $ "Dostavista API call failed: " <> ls (show apiErr)
      Right DostavistaOrdersResponse {..} ->
        case orders of
          -- CASE 1: Order Not Found
          -- The Dostavista API returned an empty 'orders' array.
          [] -> do
            $(logTM) WarningS $ "Dostavista API returned no order for ID: " <> ls (show orderId)
            -- Action: Mark this job as failed/invalid in your database.
            void $ setDostavistaOrderStatus orderId Canceled pool
              
          -- CASE 2: The "Happy Path" - Exactly one order was found
          [Order {..}] -> do -- Using RecordWildCards to bring 'status', 'courier', etc. into scope
              
            $(logTM) InfoS $ "Status for Dostavista order " <> ls  (show orderId) <> " is: " <> ls (show status)
            
            -- Only act if the status has changed
            if status /= currentStatus then do
              -- Update the status in your database
              eDbRes <- setDostavistaOrderStatus orderId status pool
              extractFromEither eDbRes $ const $ do
                modifyTVarIO statusVar $ \(_, tm) -> (status, tm)
                -- Take action based on the new status
                case status of
                  Active -> do
                    $(logTM) InfoS $ "Status changed to " <> ls (show status) <> ". a courier is assigned."
                  -- The courier is assigned! Notify the admin.
                  -- ... send notification with courier details ...
                    void $ setDostavistaPickupByCourierStatus orderId Active PickedUpByCourier pool
                    case courier of 
                      Just Courier {..} -> do
                        let templateData = 
                             HM.fromList 
                             [("orderId", tshow orderId)
                             , ("courierName", crName)
                             , ("courierSurname", fromMaybe "---" crSurname)
                             , ("courierPhone", crPhone)
                             , ("courierId", tshow crCourierId)
                             , ("deliveryCost", paymentAmount)
                             ]
                        msg <- fmap escapeMarkdownV2 $ render $currentModule templateData
                        void $ sendOrEditTelegramMessage mempty msg ORDER Nothing Nothing Nothing
                        liftIO $ threadDelay timeout
                        pollerLogic statusVar orderId
                      _ -> do
                        $(logTM) WarningS "Order is Active, but courier details were not found in the response."
                        liftIO $ threadDelay timeout
                        pollerLogic statusVar orderId
                  Completed -> do
                  -- Order is delivered. Update main order table.
                    let msg = escapeMarkdownV2 $ "the order " <> tshow orderId <> " has been delivered."
                    void $ sendOrEditTelegramMessage mempty msg ORDER Nothing Nothing Nothing
                    void $ setDostavistaOrderStatus orderId Completed pool
                  -- recurse
                  Available -> do
                    $(logTM) InfoS $ 
                      "Status changed to " <> 
                      ls (show status) <> 
                      ". starts searching for an available courier. retry in 1 min .."
                    let msg = escapeMarkdownV2 $ "the order has become visible to the courier."
                    void $ sendOrEditTelegramMessage mempty msg ORDER Nothing Nothing Nothing
                    liftIO $ threadDelay timeout
                    pollerLogic statusVar orderId
                  Canceled -> do 
                    $(logTM) InfoS $ "Dostavista order hae been cancelled.."
                    void $ setDostavistaOrderStatus orderId Canceled pool
                    msg <- fmap escapeMarkdownV2 $ render ($currentModule <> ".Cancelled") $ HM.fromList [("orderId", tshow orderId)]
                    void $ sendOrEditTelegramMessage mempty msg ORDER Nothing Nothing Nothing
                  -- Order was canceled. Revert and alert.
                  -- For 'new', 'available', etc., just log it. The next poll will check again.
                  _ -> $(logTM) ErrorS $ "Dostavista API returned an unexpected order status: " <> ls (show status)
            else do
              $(logTM) InfoS $ 
                "Status for Dostavista order " <> 
                ls (show orderId) <> 
                " has not changed. status " <> 
                ls (show status) <> 
                ". retry in 1 min .." 
              liftIO $ threadDelay timeout
              pollerLogic statusVar orderId

          -- CASE 3: Unexpected Response
          -- The API returned more than one order for a single ID query. This should not happen.
          _ -> do
              $(logTM) ErrorS $ "Dostavista API returned multiple orders for a single ID query: " <> ls  (show orderId)
              -- Action: Log this as a critical error for investigation.
              void $ setDostavistaOrderStatus orderId Canceled pool