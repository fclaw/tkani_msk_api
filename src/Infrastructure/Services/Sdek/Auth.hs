{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-} 
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE RecordWildCards     #-}

module Infrastructure.Services.Sdek.Auth (getValidSdekToken) where

import Control.Monad.State (get)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically, readTVar, writeTVar, modifyTVar')
import Katip
import Servant.Server.Internal.ServerError
import Control.Monad.Reader.Class (ask)
import Control.Monad.Error.Class
import Data.Text (Text, pack, unpack)
import Data.Aeson.TH
import GHC.Generics (Generic)
import Network.Wreq (FormParam(..)) -- Import the FormParam builder
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime)
import Data.Traversable (for)


import App
import  Infrastructure.Utils.Http (HttpError, FormParams, postFormReq)
import Text (camelToSnake, recordLabelModifier) 

data SdekTokenRaw = SdekTokenRaw
 {
   strAccessToken :: Text
 , strTokenType   :: Text
 , strExpiresIn   :: Int
 , strScope       :: Text
 , strJti         :: Text
 } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "str" } ''SdekTokenRaw)


sdekAuthUrl :: String -> String
sdekAuthUrl url = "https://" <> url <> "/v2/oauth/token" -- The sandbox URL is correct

internalGetSdekAccessToken :: SDEKCredentials -> Text -> AppM (Either HttpError SdekToken)
internalGetSdekAccessToken cred url = do
 -- Build the payload as a list of FormParams, NOT a JSON object.
  let payload :: FormParams
      payload = [ "grant_type"    := ("client_credentials" :: Text)
                , "client_id"     := sdekClientId cred
                , "client_secret" := sdekClientSecret cred
                ]
  
  -- Call our new, specialized function.
  -- It will return the parsed SdekToken on success.
  cfg <- ask
  let httpManager = _configHttpManager cfg
  sdekTokenRaw <- postFormReq @SdekTokenRaw httpManager (sdekAuthUrl (unpack url)) payload
  ct <- currentTime
  for sdekTokenRaw $ \SdekTokenRaw {..} -> do
    let sdekToken = SdekToken
          { sdekObtainedAt  = ct
          , sdekAccessToken = strAccessToken
          , sdekTokenType   = strTokenType
          , sdekExpiresIn   = strExpiresIn
          , sdekScope       = strScope
          , sdekJti         = strJti
          }
    return sdekToken

-- Helper to calculate the exact expiry time
expiryTime :: SdekToken -> UTCTime
expiryTime token =
  let seconds = fromIntegral (sdekExpiresIn token) :: NominalDiffTime
  in addUTCTime seconds (sdekObtainedAt token)


-- A safety margin. We refresh the token if it's going to expire in the next 120 seconds.
expiryMarginSeconds :: NominalDiffTime
expiryMarginSeconds = 120

-- Helper to check if a token is still fresh
isTokenFresh :: UTCTime -> SdekToken -> Bool
isTokenFresh now token =
  let expiresAt = expiryTime token
      -- Calculate how many seconds are left until it expires
      secondsLeft = diffUTCTime expiresAt now
  in secondsLeft > expiryMarginSeconds -- True if more than 2 minutes are left

getValidSdekToken :: AppM SdekToken
getValidSdekToken = do

  $(logTM) InfoS "Checking SDEK token validity..."

  stateTVar <- get
  config    <- ask
  now       <- currentTime

  -- 1. Read the current state atomically
  mbToken <- liftIO $ atomically $ fmap _sdekToken (readTVar stateTVar)

  -- 2. Check the token's validity
  case mbToken of
    -- A token exists. Is it fresh?
    Just token | isTokenFresh now token -> do
      now <- currentTime
      let secondsLeft = round $ diffUTCTime (expiryTime token) now
      $(logTM) InfoS $ ls $ "SDEK token is fresh. Reusing. Seconds left: " <> pack (show secondsLeft)
      pure token

    -- A token exists but it's old, or no token at all
    _ -> do
      $(logTM) InfoS "SDEK token is missing or expired. Refreshing..."
            
      -- Call the internal function to get a new token from SDEK's API
      eToken <- internalGetSdekAccessToken (_sdekCred config) (_sdekUrl config)      
      case eToken of
        Left err -> do
          $(logTM) ErrorS $ ls $ "Failed to fetch SDEK token: " <> show err
          throwError err500 { errBody = "SDEK authentication failed" }

        Right freshToken -> do
          -- Update the shared state with the new, fresh token
          liftIO $ atomically $ modifyTVar' stateTVar (\s -> s { _sdekToken = Just freshToken })          
          $(logTM) InfoS "Successfully refreshed SDEK token."
          pure freshToken