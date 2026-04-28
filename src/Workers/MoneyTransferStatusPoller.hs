{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module Workers.MoneyTransferStatusPoller (runMoneyTransferStatusPoller) where

import Katip
import Data.Int (Int64)
import Control.Monad (when, void)
import Data.Foldable (for_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import Data.Text (Text)

import Text (encodeToText, tshow)
import App (AppM, _appDBPool, ChatKey (MONEY_TRANSFER))
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Infrastructure.Services.Tinkoff (checkTinkoffRubleTransferStatus)
import Infrastructure.Database (fetchRubleTransferStatuses, updateRubleTransferStatus)
import Infrastructure.Services.Tinkoff.Types.RubleTransfer 
       (TransferStatus (..), RubleTransferStatusRequest (..), RubleTransferStatusResponse (..))


send msg = void $ sendOrEditTelegramMessage mempty (escapeMarkdownV2 msg) MONEY_TRANSFER Nothing Nothing Nothing

logErr msg =  $(logTM) ErrorS (ls msg) >> send msg

runMoneyTransferStatusPoller :: AppM ()
runMoneyTransferStatusPoller = do
  $(logTM) InfoS "Polling for money transfer statuses..."
  cfg <- ask
  eDbRes <- fetchRubleTransferStatuses (_appDBPool cfg)
  case eDbRes of
    Left err -> $(logTM) ErrorS $ "Error occurred while fetching transfer statuses: " <> ls err
    Right statuses ->
      for_ statuses $ \(ident, transferId, amount) -> do
        let req = RubleTransferStatusRequest transferId
        eStatus <- checkTinkoffRubleTransferStatus req
        case eStatus of
          Right (Right RubleTransferStatusResponse{..}) -> do
            when(rtsrStatus /= IN_PROGRESS) $ do
              $(logTM) InfoS $ "Updating transfer status for ID: " <> ls (tshow ident)
              void $ updateRubleTransferStatus ident (encodeToText rtsrStatus) (_appDBPool cfg)
              send $ "Transfer of " <> tshow amount <> " RUB has been completed"
          Right (Left err) -> logErr $ "Error occurred while checking transfer status for ID: " <> tshow ident <> ", error: " <> tshow err
          Left err -> logErr $ "Error occurred while checking transfer status for ID: " <> tshow ident <> ", error: " <> tshow err