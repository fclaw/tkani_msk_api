{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Workers.TinkoffShipmentPaymentStatusPoller (runTinkoffShipmentPaymentStatusPoller) where

import Katip
import Data.Text (Text)
import Data.Int (Int64)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Control.Monad.State.Class (get)
import Control.Monad.Reader.Class (ask)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import System.Timeout.Lifted (timeout)
import Data.Function (fix)
import qualified Data.Map.Strict as M
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime)
import Data.Traversable (for)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.Async.Lifted (async)
import Control.Monad (forever, void, when)
import Network.Wreq hiding (get)
import Data.Aeson.KeyMap as A
import Data.Aeson as A
import Control.Lens ((&), (.~), (^.), (?~))
import Network.HTTP.Client (HttpException (..))
import Control.Exception (SomeException (..), fromException)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL

import App
import Text (tshow)
import Domain.Error (handleHttpError)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Tinkoff.Types.GetState
import Concurrency (runJobWithCleanup)
import TH.Location (currentModule)
import Infrastructure.Services.Tinkoff.Types.Cancel
import Infrastructure.Database (fetchPendingShipmentPayments, updateShipmentPaymentStatus, setIsShipmentPaid)
import Workers.TinkoffPaymentStatusPoller (getAdaptiveDelay, resendFinalizedMessage, delayFast, delayMedium, delaySlow)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, ParseMode (MarkdownV2), try', TelegramError (ApiRequestFailed))
import Infrastructure.Services.Tinkoff (checkTinkoffPaymentStatus, cancelTinkoffPayment)
import Infrastructure.Services.Tinkoff.Security (generateGetStateToken, GetStateToken(..))


runTinkoffShipmentPaymentStatusPoller :: AppM ()
runTinkoffShipmentPaymentStatusPoller = do
  -- Run the core logic within our application's monad to get access to the DB, logger, etc.
  $(logTM) InfoS "Polling for Tinkoff shipment payment statuses..."
  stvar <- get
  -- Get the channel from the application state.
  st <- readTVarIO stvar
  let chan = _tinkoffShipmentChan st
  
  -- fetch pending payments and enqueue them
  pool <- fmap _appDBPool ask
  ePendingPayments <- fetchPendingShipmentPayments pool
  for_ ePendingPayments $ \xs -> do
    tinkoffCred <- fmap _tinkoffCred ask
    for_ xs $ \(orderId, paymentId, chatId, messageId) -> do
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
      let job = TinkoffShipmentPaymentJob orderId getStateRequest chatId messageId
      liftIO $ STM.atomically $ STM.writeTChan chan job
      $(logTM) InfoS $ ls $ "enqueued pending shipment payment for Order " <> orderId

  -- Loop forever, dispatching a new worker for each job that arrives.
  forever $ do
    job@TinkoffShipmentPaymentJob{tspjOrderId} <- readTChanIO chan
    $(logTM) InfoS $ ls $ "Dispatching worker for Order " <> tspjOrderId
    void $ async $ runJobWithCleanup $ workerLogic job



sendMsgToPrepaidOrderChannel msg = void $ sendOrEditTelegramMessage mempty msg PREPAID_ORDER Nothing Nothing Nothing

workerLogic :: TinkoffShipmentPaymentJob -> AppM ()
workerLogic TinkoffShipmentPaymentJob {..} = do
  startTime <- currentTime
  -- Set the hard limit (20 minutes = 1200 seconds)
  -- Using timeout to kill this specific thread if it runs too long
  let twentyMinutesMicros = 20 * 60 * 1000000

  mResult <- timeout twentyMinutesMicros $ fix $ \loop -> do
    -- A. Check Time Elapsed
    now <- currentTime
    let elapsed = diffUTCTime now startTime
    -- B. Call Tinkoff API
    eStatus <- checkTinkoffPaymentStatus tspjStateRequest
    case eStatus of
      Left err -> do
        $(logTM) ErrorS $ 
          "API error while checking shipment \
          \ payment status for Order " <> 
          ls tspjOrderId <> ": " <> ls (tshow err)
        let errorMsg = 
               escapeMarkdownV2 $ 
                 "‼️ Error in calling \
                 \ checkTinkoffPaymentStatus: " <> 
                 (tshow err)
        sendMsgToPrepaidOrderChannel errorMsg
        let sleepTime = getAdaptiveDelay elapsed
        liftIO (threadDelay sleepTime) >> loop -- RECURSIVE CALL
      Right status ->
        case status of
          s | s `elem` [CONFIRMED, AUTHORIZED] -> do
            pool <- fmap _appDBPool ask
            void $ updateShipmentPaymentStatus tspjOrderId s pool (Just setIsShipmentPaid)
            sendPaymentResultToUser tspjOrderId tspjChatId tspjMessageId "Confirmed"
            -- EXIT LOOP
          s | s `elem` [REJECTED, REVERSED]    -> do
            pool <- fmap _appDBPool ask
            void $ updateShipmentPaymentStatus tspjOrderId REJECTED pool Nothing
            let rejectedMsg = 
                  escapeMarkdownV2 $ 
                    "‼️ payment for order " <> 
                    tspjOrderId <> 
                    " was rejected"
            sendMsgToPrepaidOrderChannel rejectedMsg
            sendPaymentResultToUser tspjOrderId tspjChatId tspjMessageId "Rejected"
          -- EXIT LOOP
          DEADLINE_EXPIRED                     -> do
            pool <- fmap _appDBPool ask
            void $ updateShipmentPaymentStatus tspjOrderId CANCELLED pool Nothing
            let deadlineMsg = 
                  escapeMarkdownV2 $ 
                    "‼️ payment for order " <> 
                    tspjOrderId <> 
                    " was timed out"
            sendMsgToPrepaidOrderChannel deadlineMsg
            sendPaymentResultToUser tspjOrderId tspjChatId tspjMessageId "Deadline"
          -- EXIT LOOP
          CANCELLED                            -> do
            pool <- fmap _appDBPool ask
            void $ updateShipmentPaymentStatus tspjOrderId CANCELLED pool Nothing
            let cancelledMsg = 
                  escapeMarkdownV2 $ 
                    "‼️ payment for order " <> 
                    tspjOrderId <> 
                    " was cancelled"
            sendMsgToPrepaidOrderChannel cancelledMsg
            sendPaymentResultToUser tspjOrderId tspjChatId tspjMessageId "Cancelled"
          -- EXIT LOOP
          ------------------------------------------------------------
          --  CONTINUE POLLING (New, Processing, Unknown)
          ------------------------------------------------------------
          other -> do
            case other of
              UNKNOWN_STATUS t -> 
                $(logTM) WarningS $ ls $ 
                  "Unknown status for " <> tspjOrderId <> ": " <> t
              _         -> return ()
            let sleepTime = getAdaptiveDelay elapsed
            liftIO (threadDelay sleepTime) >> loop -- RECURSIVE CALL

  -- Handle Timeout Case (if result is Nothing)
  when(isNothing mResult) $ do
    $(logTM) WarningS $ ls $ "Order " <> tspjOrderId <> " TIMED OUT."
    sendPaymentResultToUser tspjOrderId tspjChatId tspjMessageId "Deadline"

    let makeCancelReq = 
         CancelRequest 
         { cTerminalKey = gsrqTerminalKey tspjStateRequest
         , cPaymentId   = gsrqPaymentId tspjStateRequest
         , cToken       = gsrqToken tspjStateRequest
         }
    
    eTinkoffResp <- cancelTinkoffPayment makeCancelReq
    for_ eTinkoffResp $ \CancelResponse {..} ->
      if cSuccess then 
        $(logTM) InfoS $ "tinkoff payment has been cancelled due to timeout"
      else do
        let errorMsg = 
              escapeMarkdownV2 $
              "‼️ tinkoff cancel order failed. \
              \ manual intervention is urgently required!! " <>
              decodeUtf8 (BL.toStrict (encodePretty makeCancelReq))     
        sendMsgToPrepaidOrderChannel errorMsg
    pool <- fmap _appDBPool ask
    void $ updateShipmentPaymentStatus tspjOrderId CANCELLED pool Nothing


sendPaymentResultToUser :: Text -> Int64 -> Int64 -> Text -> AppM ()
sendPaymentResultToUser orderId chatId messageId status = do
  bots <- fmap _bots ask
  let botsInfo = M.lookup CONCIERGE bots
  for_ botsInfo $ \(bot, _) -> do
    let templateData = HM.fromList [("orderId", orderId)]
    msg <- fmap escapeMarkdownV2 $ render ($currentModule <> "." <> status) templateData
    httpManager <- fmap _configHttpManager ask -- Assumes Manager is in your Config
    let url = "https://api.telegram.org/bot" <> T.unpack bot <> "/sendMessage"
    let payload = A.Object $ A.fromList
          [ "chat_id"             A..= tshow chatId
          , "text"                A..= msg
          , "parse_mode"          A..= tshow MarkdownV2
          , "reply_to_message_id" A..= tshow messageId
          ]
    eTelResp <- liftIO $ try' $ postWith (defaults & manager .~ Right httpManager) url payload
    when(isLeft eTelResp) $ do
      let Left someEx = eTelResp
      case fromException @HttpException someEx of 
        Nothing -> $(logTM) ErrorS $ ls $ "failed to send the message " <> show someEx
        Just httpErr -> do
          msg <- fmap _messageNotFound ask
          let errorText = handleHttpError httpErr
          if msg `T.isInfixOf` errorText
          then do
            -- Handle the fallback logic as before
            $(logTM) WarningS $ ls $ "Could not edit message for " <> orderId <> ". Reason: " <> errorText
            -- ... send a new message ...
            resendFinalizedMessage orderId chatId msg CONCIERGE
          else
            -- It's a different, more serious error.
            $(logTM) CriticalS $ ls $ "CRITICAL: Failed to send notification for " <> orderId <> ". " <> errorText