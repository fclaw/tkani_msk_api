{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Types
  ( AppState(..)
  , AppM(..)
  ) where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, local)
import Servant (Handler, ServerError)
import Control.Monad.Except (MonadError)
import Hasql.Pool (Pool)

-- Katip imports
import Katip (Katip(..), KatipContext(..), LogEnv, Namespace, LogContexts)
import Data.Aeson (Value)
import Control.Applicative (pure)
import Data.Monoid (mempty)


-- | AppState holds all the shared, read-only resources for our application.
data AppState = AppState
  { appDBPool :: Pool
  , appLogEnv :: LogEnv
  }

-- | AppM is our application's custom monad.
newtype AppM a = AppM { unAppM :: ReaderT AppState Handler a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppState
    , MonadError ServerError
    )

-- | INSTANCE FOR KATIP LOGGING (Corrected)
instance Katip AppM where
  getLogEnv = asks appLogEnv
  -- --- THE FIX for 'localLogEnv' ---
  localLogEnv f (AppM m) = AppM (local (\s -> s { appLogEnv = f (appLogEnv s) }) m)

-- | INSTANCE FOR KATIP CONTEXT (Corrected)
instance KatipContext AppM where
  getKatipContext   = pure mempty
  getKatipNamespace = pure mempty

  -- --- THE FIX for 'localKatipContext' and 'localKatipNamespace' ---
  -- Since we don't have a dynamic context, these 'local' functions do nothing.
  -- They simply run the computation without changing the context.
  localKatipContext :: (LogContexts -> LogContexts) -> AppM a -> AppM a
  localKatipContext _ m = m

  localKatipNamespace :: (Namespace -> Namespace) -> AppM a -> AppM a
  localKatipNamespace _ m = m