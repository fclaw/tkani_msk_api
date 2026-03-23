{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Workers.StuckOrdersWatcher (runStuckOrdersWatcher) where

import Katip
import Data.Text (Text, unpack)
import Data.Int (Int64)
import Data.Either (isLeft)
import Control.Monad (void, when)
import Data.Foldable (for_)
import Control.Monad.Reader.Class (ask)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=), object)
import Network.Wreq (postWith, defaults, manager)
import Control.Lens ((&), (.~))

import Text (tshow)
import TH.Location (currentModule)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Workers.SimpleOrderOrchestrator (try')
import App (AppM, _appDBPool, ChatKey (ORDER, MAIN), render, _bots, _configHttpManager)
import Infrastructure.Database (collectOrdersStuckInPaid)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, ParseMode(MarkdownV2))


runStuckOrdersWatcher :: AppM ()
runStuckOrdersWatcher = do
  cfg <- ask
  let pool = _appDBPool cfg
  eStallOrders <- collectOrdersStuckInPaid pool
  case eStallOrders of 
    Left err -> 
      $(logTM) ErrorS $ 
        "collectOrdersStuckInPaid db failure " <> 
        ls (tshow eStallOrders)
    Right stallOrders ->
      for_ stallOrders $ \order ->  do
        uncurry notifyCustomer order
        let warnMsg = escapeMarkdownV2 $ "⚠️ order is stall: " <> fst order
        void $ sendOrEditTelegramMessage mempty warnMsg ORDER Nothing Nothing Nothing

notifyCustomer :: Text -> Int64 -> AppM ()
notifyCustomer orderId chatId = do
  bots <- fmap _bots ask
  let (bot,_) = (M.!) bots MAIN
  let url = "https://api.telegram.org/bot" <> unpack bot <> "/sendMessage"
  let templateData = HM.fromList [ ("orderId", orderId) ]
  message <- fmap escapeMarkdownV2 $ render $currentModule templateData
  httpManager <- fmap _configHttpManager ask
  let payload =
        object
        [ "chat_id"     .= chatId
        , "text"        .= message
        , "parse_mode"  .= tshow MarkdownV2
        ]
  eTelMsgId <- liftIO $ try' $ postWith (defaults & manager .~ Right httpManager) url payload
  when(isLeft eTelMsgId) $ $(logTM) ErrorS $ "notifyCustomer telegram error " <> ls (tshow eTelMsgId)
