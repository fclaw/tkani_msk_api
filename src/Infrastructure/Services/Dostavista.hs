{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell   #-}

module Infrastructure.Services.Dostavista (scheduleDostavistaPickup, getOrder, cancelOrder) where

import Katip
import Data.Int (Int64)
import qualified Data.Text as T
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)


import Text (tshow)
import Infrastructure.Utils.Http (HttpError, postReq, getReq, Token (..))
import Infrastructure.Services.Dostavista.Types
import Infrastructure.Services.Dostavista.Types.Error
import qualified Infrastructure.Services.Dostavista.Types.Config as Cfg
import App (AppM, _dostavistaConfig, _configHttpManager, Scheme (HTTPS))


scheduleDostavistaPickup :: Int -> AppM (Either HttpError DostavistaOrderResponse)
scheduleDostavistaPickup totalWeightGrams = do
  cfg <- ask
  let mgr = _configHttpManager cfg
  let dostavistaCfg = _dostavistaConfig cfg
  let url = show HTTPS <> T.unpack (Cfg.url dostavistaCfg) <> "/create-order"
  let contact = 
        DostavistaContact 
        (Cfg.name (Cfg.contact dostavistaCfg)) 
        (Cfg.phone (Cfg.contact dostavistaCfg))
  let start = 
        DostavistaPoint 
        (Cfg.address (Cfg.source dostavistaCfg)) 
        contact 
        (Cfg.latitude (Cfg.source dostavistaCfg)) 
        (Cfg.longitude (Cfg.source dostavistaCfg))
  let end = 
        DostavistaPoint 
        (Cfg.address (Cfg.destination dostavistaCfg)) 
        contact
        (Cfg.latitude (Cfg.destination dostavistaCfg)) 
        (Cfg.longitude (Cfg.destination dostavistaCfg))
  let orderReq = 
        defDostavistaOrderRequest 
        { drTotalWeightKg = fromIntegral totalWeightGrams / 1000
        , drPoints = [start, end]
        }
  let token = Token "X-DV-Auth-Token" (Cfg.token dostavistaCfg)
  eResp <- postReq @DostavistaOrderResponse mgr url orderReq (Just token)
  fmap (const eResp) $ $(logTM) InfoS $ "Dostavista order registration response: " <> ls (show eResp)


getOrder :: Int64 -> AppM (Either HttpError DostavistaOrdersResponse)
getOrder orderId = do
  cfg <- ask
  let mgr = _configHttpManager cfg
  let dostavistaCfg = _dostavistaConfig cfg
  let url = show HTTPS <> T.unpack (Cfg.url dostavistaCfg) <> "/orders"
  let token = Token "X-DV-Auth-Token" (Cfg.token dostavistaCfg)
  eResp <- getReq @DostavistaOrdersResponse mgr url [("order_id", tshow orderId)] (Just token)
  fmap (const eResp) $ $(logTM) InfoS $ "Dostavista order poller response: " <> ls (show eResp)

cancelOrder :: Int64 -> AppM (())
cancelOrder orderId = do
  cfg <- ask
  let mgr = _configHttpManager cfg
  let dostavistaCfg = _dostavistaConfig cfg
  let url = show HTTPS <> T.unpack (Cfg.url dostavistaCfg) <> "/cancel-order"
  let token = Token "X-DV-Auth-Token" (Cfg.token dostavistaCfg)
  let cancelReq = CancelOrderRequest orderId
  eResp <- postReq @DostavistaOrderResponse mgr url cancelReq (Just token)
  $(logTM) InfoS $ "Dostavista cancel order response: " <> ls (show eResp)