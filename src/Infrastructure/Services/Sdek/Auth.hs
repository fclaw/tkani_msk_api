{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-} 
{-# LANGUAGE TypeApplications     #-}

module Infrastructure.Services.Sdek.Auth (getValidSdekToken) where

import App
import Control.Monad.State (get)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Katip
import Servant.Server.Internal.ServerError
import Control.Monad.Reader.Class (ask)
import Control.Monad.Error.Class
import Data.Text (Text, pack, unpack)
import Data.Aeson.TH
import GHC.Generics (Generic)
import Network.Wreq (FormParam(..)) -- Import the FormParam builder


import  Infrastructure.Utils.Http (HttpError, FormParams, postFormReq)
import Text (camelToSnake, recordLabelModifier) 


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
  postFormReq @SdekToken (sdekAuthUrl (unpack url)) payload

-- This is the function we sketched out before, now fully integrated.
getValidSdekToken :: AppM SdekToken
getValidSdekToken = do
  stateTVar <- get -- From MonadState
  config <- ask -- From MonadReader
  -- Atomically check the current token
  state <- liftIO $ atomically $ readTVar stateTVar
  let maybeToken = _sdekToken state
  case maybeToken of
    -- TODO: Add expiry check here in a real app
    Just validToken -> pure validToken

    Nothing -> do
      -- Token is missing, fetch a new one
      $(logTM) InfoS "SDEK token is missing or expired. Refreshing..."
      eToken <- internalGetSdekAccessToken (_sdekCred config) (_sdekUrl config)
      case eToken of
        Left err -> do
          $(logTM) ErrorS $ logStr ("error during fetching token  " <> pack (show err))
          throwError err500 { errBody = "SDEK auth failed" }
        Right newToken -> do
          -- Update the shared state with the new token
          liftIO $ atomically $ writeTVar stateTVar (state { _sdekToken = Just newToken })
          pure newToken