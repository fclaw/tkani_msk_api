{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}

module App
  ( State(..)
  , AppM(..),
  Config (..),
  SdekToken (..),
  SDEKCredentials (..),
  MetroCity (..),
  currentTime,
  render,
  runAppM
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
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS (runRWST)
import Data.Time (UTCTime)
import qualified Data.HashSet as HS
import Control.Concurrent (MVar)
import Data.UUID (UUID)

-- Katip imports
import Katip
import Data.Aeson (Value, FromJSON, parseJSON, withObject, (.:))
import Control.Applicative (pure)
import Data.Monoid (mempty)
import Text (recordLabelModifier)
import API.Types (ProviderInfo, DeliveryPoint)
import Infrastructure.Templating (TemplateMap, renderTemplate, TemplateData)
import API.WithField (WithField)
import Infrastructure.Services.Sdek.Types (SdekConfirmation, SdekError)


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

type CityName = Int
type CachedPoints = (UTCTime, [WithField "dpMetros" [Text] DeliveryPoint]) -- (Timestamp of when it was cached, The data)
type PointCache = HM.HashMap CityName CachedPoints


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


-- A type to represent the final result from the poller.
data SdekFinalResult = 
       SdekResultSuccessful SdekConfirmation 
     | SdekResultInvalid [SdekError]
  deriving (Show)

-- The map from SDEK's tracking UUID to the MVar "promise".
type SdekPromiseMap = HM.HashMap UUID (MVar SdekFinalResult)

-- This will be our mutable, thread-safe state.
-- It holds the SDEK token and its expiry time.
data State = State
  { _sdekToken :: Maybe SdekToken -- Stored in a TVar for thread safety
  , _pointCache :: PointCache
  , _sdekPromises :: SdekPromiseMap
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
  , _configYandexApiKey :: Text
  , _metroCityCodes :: HS.HashSet Int
  }

-- A helper type for parsing the YAML
newtype MetroCity = MetroCity { code :: Int }
instance FromJSON MetroCity where
  parseJSON = withObject "MetroCity" $ \v -> MetroCity <$> v .: "code"

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

-- === MANUAL INSTANCES for MonadBaseControl ===

-- INSTANCE 1: MonadBase IO AppM
-- This instance teaches the compiler how to get from the base monad (IO)
-- into our AppM stack. It's the superclass required by MonadBaseControl.
instance MonadBase IO AppM where
    -- Lifting from the base is just lifting from IO.
    liftBase = liftIO

-- INSTANCE 2: MonadBaseControl IO AppM
-- This is the core instance. It teaches the compiler how to "unlift" AppM
-- back down to IO, run an action, and restore the monadic state.
instance MonadBaseControl IO AppM where
    -- The state of the monadic computation is the same as the state of the inner stack.
    type StM AppM a = StM (RWST Config [Text] (TVar State) (ExceptT ServerError IO)) a

    -- How to lift an action that operates in the base monad.
    -- We are delegating this to the instance for RWST, just wrapping/unwrapping our newtype.
    liftBaseWith f = AppM $ liftBaseWith $ \runInBase -> f (runInBase . unAppM)

    -- How to restore the monadic state.
    -- Again, we just delegate to the underlying RWST instance.
    restoreM = AppM . restoreM

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

  -- ADD THIS RUNNER FUNCTION: This is our bridge from AppM to IO
-- It will be used by main to run the workers.
runAppM :: Config -> TVar State -> AppM a -> IO (Either ServerError a)
runAppM config stateTVar appM =
  -- Unwind the monad stack to get to the base IO.
  runExceptT . fmap (\(a, _, _) -> a) $ runRWST (unAppM appM) config stateTVar