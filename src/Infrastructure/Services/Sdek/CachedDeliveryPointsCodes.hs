{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Infrastructure.Services.Sdek.CachedDeliveryPointsCodes (fetchCodes) where


import Katip
import Data.Text (Text)
import Control.Monad (void)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically, modifyTVar')

import App
import Infrastructure.Utils.Http
import TH.Location (currentModule)
import Infrastructure.Services.Sdek.Auth (getValidSdekToken)
import qualified Infrastructure.Services.Sdek.Types.Config as Sdek (url)



fetchCodes :: AppM [SdekPointCode]
fetchCodes = do
  now <- currentTime
  stateTVar <- get
  maybeCodes <- fmap _sdekPointsCodes $ readTVarIO stateTVar
  case maybeCodes of 
    Nothing -> fetchAndCache
    Just (cachedTime, codes) -> do
      if diffUTCTime now cachedTime < 864000 -- once in 10 days
      then return codes
      else fetchAndCache

fetchAndCache :: AppM [SdekPointCode]
fetchAndCache = do
  $(logTM) InfoS $ "fetching SDEK points codes"
  cfg <- ask
  let url = (T.unpack . Sdek.url . _sdekConfig) cfg
  let httpManager = _configHttpManager cfg
  let pointsUrl = show HTTPS <> url <> "/v2/deliverypoints"
  let pointsReq = 
        getValidSdekToken >>= \token -> 
          let tkn =  (Just . mkDefToken . sdekAccessToken) token
          in _getReq' httpManager pointsUrl [] [] tkn
  let auth = Just (void $ getValidSdekToken)
  ePoints <- makeRequestWithRetries @[SdekPointCode] auth pointsReq
  handleApiResponse $(currentModule) ePoints $ \codes -> do
    now <- currentTime
    stateTVar <- get
    fmap (const codes) $ modifyTVarIO stateTVar $ \s -> s { _sdekPointsCodes = Just (now, codes) }