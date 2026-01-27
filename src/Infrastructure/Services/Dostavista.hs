{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell   #-}

module Infrastructure.Services.Dostavista (scheduleDostavistaPickup, getOrder, cancelOrder) where

import Katip
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeZone(..), ZonedTime(..), getZonedTime, utcToZonedTime)


import Text (tshow, textToInt)
import Infrastructure.Utils.Http (HttpError, postReq, getReq, Token (..))
import Infrastructure.Services.Dostavista.Types
import Infrastructure.Services.Dostavista.Types.Error
import qualified Infrastructure.Services.Dostavista.Types.Config as Cfg
import App (AppM, _dostavistaConfig, _configHttpManager, Scheme (HTTPS))



-- | A robust function to calculate a future time and format it correctly.
calculateAndFormatDropOffTime :: IO Text
calculateAndFormatDropOffTime = do
  -- --- 1. Define Timezone and Duration ---

  -- Your manual MSK timezone. This is fine, but see notes below.
  let msk = TimeZone (3 * 60) False "MSK"
  
  -- The duration to add (5400 seconds = 1.5 hours)
  let durationToAdd :: NominalDiffTime
      durationToAdd = fromInteger 5400

  -- --- 2. Perform the Calculation ---

  -- Get the current time as a universal, timezone-agnostic UTCTime.
  nowUTC <- getCurrentTime
  
  -- Add the duration to the UTCTime. This is the correct way to do time arithmetic.
  let futureUTC = addUTCTime durationToAdd nowUTC
  
  -- Now, create the ZonedTime object for the *final, future* moment in MSK.
  let futureZonedInMSK = utcToZonedTime msk futureUTC

  -- --- 3. Format the Result ---
  
  -- We use the same robust formatting logic from our previous discussion,
  -- as it's guaranteed to produce the required format with the colon.
  let baseFormat = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" futureZonedInMSK
  let offset = T.pack $ formatTime defaultTimeLocale "%z" futureZonedInMSK
  let (offsetHours, offsetMinutes) = T.splitAt 3 offset
  
  let postFormatString = baseFormat <> offsetHours <> ":" <> offsetMinutes

  pure postFormatString


mkMessage numPackages totalWeight = "Забрать " <> numPackages <> " упаковок с тканью (общий вес " <> totalWeight <> " кг) и доставить в ближайший пункт СДЭК"


scheduleDostavistaPickup :: [DostavistaPackage] -> Int -> AppM (Either HttpError DostavistaOrderResponse)
scheduleDostavistaPickup packages totalWeightGrams = do

  dropOffTime <- liftIO calculateAndFormatDropOffTime

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
        (Just dropOffTime)
        packages
  let end = 
        DostavistaPoint 
        (Cfg.address (Cfg.destination dostavistaCfg))
        contact
        (Cfg.latitude (Cfg.destination dostavistaCfg)) 
        (Cfg.longitude (Cfg.destination dostavistaCfg))
        Nothing
        []
  let orderReq = 
        defDostavistaOrderRequest 
        { drTotalWeightKg   = fromIntegral totalWeightGrams / 1000
        , drPoints          = [start, end]
        , drMatter          = mkMessage (tshow (length packages)) (tshow (fromIntegral totalWeightGrams / 1000))
        , drInsuranceAmount = tshow (sum $ map (fromMaybe 0 . textToInt . pkgItemPaymentAmount) packages) <> ".00" 
        }
  $(logTM) InfoS $ ls $ "orderReq: " <> encodePretty orderReq
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