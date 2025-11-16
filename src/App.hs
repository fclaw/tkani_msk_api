{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}

module App
  ( State(..)
  , AppM(..),
  Config (..),
  SdekToken (..),
  SDEKCredentials (..),
  currentTime,
  render
  ) where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, local)
import Servant (Handler, ServerError)
import Control.Monad.Except (MonadError, ExceptT)
import Hasql.Pool (Pool)
import Data.Text (Text, pack)
import Control.Concurrent.STM (TVar)
import Control.Monad.RWS (RWST, MonadState, withRWST) -- Important
import Control.Lens
import GHC.Generics (Generic)
import Data.Aeson.TH
import Control.Monad.Catch
import Control.Monad.Time
import Network.HTTP.Client (Manager)
import Language.Haskell.TH (loc_module, location)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

-- Katip imports
import Katip
import Data.Aeson (Value)
import Control.Applicative (pure)
import Data.Monoid (mempty)
import Text (recordLabelModifier)
import API.Types (ProviderInfo)
import Infrastructure.Templating (TemplateMap, renderTemplate, TemplateData)


-- "access_token": "string",
-- "token_type": "string",
-- "expires_in": 0,
-- "scope": "string",
-- "jti": "string"
data SdekToken = SdekToken
 {
   sdekAccessToken :: Text
 , sdekTokenType   :: Text
 , sdekExpiresIn   :: Int
 , sdekScope       :: Text
 , sdekJti         :: Text
 } deriving (Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "sdek" } ''SdekToken)


-- The magical rendering function
render :: (MonadReader Config m, MonadIO m) => Text -> TemplateData -> m Text
render currentModule templateData = do
  tplMap <- asks configTemplateMap
  case HM.lookup currentModule tplMap of
    Nothing ->
      -- This will be a compile-time or runtime error depending on usage,
      -- but it clearly signals a missing template.
      error $ "Template not found for module: " ++ T.unpack currentModule
    Just template ->
      pure $ renderTemplate template templateData

-- This will be our mutable, thread-safe state.
-- It holds the SDEK token and its expiry time.
data State = State
  { _sdekToken :: Maybe SdekToken -- Stored in a TVar for thread safety
  }

-- | AppState holds all the shared, read-only resources for our application.
data Config = Config
  { _appDBPool :: Pool
  , _appLogEnv :: LogEnv
  , _providers :: [ProviderInfo]
  , _sdekCred  :: SDEKCredentials
  , _sdekUrl   :: Text
  , _configBotToken :: Text
  , _orderChatId :: Text
  , _configHttpManager :: Manager
  , configTemplateMap :: TemplateMap
  }

 -- Construct the SdekCreds record, converting String to Text
data SDEKCredentials = SdekCreds
  { sdekClientId :: Text
  , sdekClientSecret :: Text
  }

makeLenses ''Config

-- The 'AppM' Monad is now an RWST stack over Handler
-- R: Reader for Config (read-only)
-- W: Writer for logs (we can use a list of Text for simplicity)
-- S: State for AppState (read-write)
newtype AppM a = AppM
  { unAppM :: RWST Config [Text] (TVar State) (ExceptT ServerError IO) a
  } deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader Config         -- Can read from 'Config'
      , MonadState (TVar State) -- Can read/write the TVar 'AppState'
      , MonadError ServerError     -- Can throw Servant errors
      , MonadThrow -- New
      , MonadCatch -- New
      , MonadTime
      )
    via (RWST Config [Text] (TVar State) (ExceptT ServerError IO))

-- | INSTANCE FOR KATIP LOGGING (Corrected for RWST)
instance Katip AppM where
  getLogEnv = asks _appLogEnv
  -- We still use a lens to modify OUR OWN Config type. This part is correct.
  localLogEnv f (AppM m) = AppM $ withRWST (\r s -> (over appLogEnv f r, s)) m

-- This is the correct, simple implementation for a monad stack like ours
-- that does not manage its own separate log context.
instance KatipContext AppM where
  -- Get the current context and namespace. Since we don't store them, they are empty.
  getKatipContext   = pure mempty
  getKatipNamespace = pure mempty
  -- Locally modify the context/namespace for a computation 'm'.
  -- Because our monad doesn't have a place to store this information,
  -- we simply run the original computation 'm' without changing anything.
  -- Katip's internal machinery will handle the rest.
  localKatipContext _ m = m
  localKatipNamespace _ m = m