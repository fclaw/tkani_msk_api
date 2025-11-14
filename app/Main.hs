{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant (Handler)
import Servant.Server
import Servant.Server.Generic
import Servant.API.Generic (toServant)
-- Database and logging imports
import qualified Hasql.Pool as Pool
import Katip
import Control.Monad.Reader (runReaderT)
import qualified Hasql.Pool.Config as Config
import Hasql.Connection.Setting (connection)
import Hasql.Connection.Setting.Connection (string)
import Control.Monad (void)
import Control.Exception (finally, bracket)
import Network.Wai.Middleware.Cors (simpleCors) -- Import the middleware
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import GHC.IO.Exception (userError)
import Control.Monad.Error.Class (throwError)
import System.Environment (getEnv)
import Data.Text (pack)
import Control.Concurrent.STM (TVar, atomically, newTVarIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS (runRWST)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import System.IO (stdout)
import qualified Data.Text.IO as TIO
import Control.Monad.Catch (onException)
import Data.Text (Text)


import Types (AppM(..))
import Handlers (apiHandlers) -- Import our top-level record of handlers
import Config (loadConfig, AppConfig(..))

import API.Types (ProviderInfo)
import Types (SDEKCredentials (..), Config (..), State (..))
import API (tkaniApiProxy)
import Logging.Telegram (mkTelegramScribe, getTelegramConfig)


handleProvidersResult (Right providers) go = go providers
handleProvidersResult (Left error) _ = throwError $ userError ("cannot open providers.yaml: " <> prettyPrintParseException error)


-- This is the "natural transformation" that converts our 'AppM' into a 'Handler'
nt :: forall a . Config -> TVar State -> AppM a -> Handler a
nt config stateTVar appM = do
  -- Run the RWST computation
  -- It gives us the result 'a', the final state 's', and the writer output 'w'
  let shutDownNotice = $(logTM) EmergencyS $ logStr @Text "server is about to being shut down"
  let runMonadsStack = runRWST (unAppM (appM `onException` shutDownNotice)) config stateTVar;
  result <- liftIO $ runExceptT $ do (a, _, _) <- runMonadsStack; return a
  -- Handle the result of the ExceptT
  case result of
    Left err -> throwError err -- Propagate Servant errors
    Right a  -> pure a         -- Return the successful result

-- The app function now needs to create the initial state TVar in IO
runApp :: Config -> IO Application
runApp config = do
  -- Create the initial mutable state in an IO transaction
  initialState <- newTVarIO $ State { _sdekToken = Nothing }
  -- Define the server with the hoisted handlers
  let server = hoistServer tkaniApiProxy (nt config initialState) (toServant apiHandlers)
  -- simpleCors is middleware, it should wrap the application
  pure $ simpleCors $ serve tkaniApiProxy server


-- | A helper function to set up and tear down the Katip LogEnv
withLogEnv :: (LogEnv -> IO a) -> IO a
withLogEnv action = do
  -- 1. Create a handle scribe that logs to stdout.
  --    'makeHandleScribe' takes a handle, severity level, and verbosity.
  --    'ColorIfTerminal' will colorize logs if writing to a TTY.
  --    'V2' is a good default verbosity level.
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2

  -- 2. Create the initial LogEnv.
  --    "tkani-api" is the application name.
  --    "production" is the environment name.
  --    These will be part of every log message.
  initialLogEnv <- initLogEnv "tkani-api" "production"

  -- 3. Register the stdout scribe. We can use defaultScribeSettings.
  let logEnvWithStdout = registerScribe "stdout" handleScribe defaultScribeSettings initialLogEnv

  -- 4. Attempt to create and register the Telegram scribe.
  mTelegramConfig <- getTelegramConfig
  logEnvWithTelegram <- case mTelegramConfig of
    Nothing -> do
      TIO.putStrLn "--> TELEGRAM_BOT_TOKEN or TELEGRAM_CHAT_ID not set. Skipping Telegram logger."
      pure logEnvWithStdout
    Just config -> do
      TIO.putStrLn "--> Telegram logger configured. Initializing scribe."
      -- Create the scribe, passing the minimum severity (e.g., InfoS) directly.
      -- This severity is now baked into the scribe itself.
      telegramScribe <- mkTelegramScribe config InfoS V2
      -- Register it using the default settings.
      pure $ logEnvWithStdout >>= registerScribe "telegram" telegramScribe defaultScribeSettings
  
  let finalLogEnv = logEnvWithTelegram

  -- 5. Use 'bracket' to ensure scribes are closed properly.
  bracket finalLogEnv closeScribes action

main :: IO ()
main = withLogEnv $ \logEnv -> do
  providersResult <- decodeFileEither @[ProviderInfo] "providers.yaml"
  handleProvidersResult providersResult $ \providers_ -> do

    -- 1. Load configuration from environment variables
    config <- loadConfig

    -- 3. Define the connection string.
    let connString = string (configDBConnString config)
    
    -- 4. Build the configuration using the DSL.
    let poolConfig = Config.settings
          [ Config.size 10                         -- Pool size of 10
          , Config.acquisitionTimeout 10           -- Timeout of 10 seconds
          , Config.staticConnectionSettings [connection connString] -- The connection string itself
          ]

    -- 5. Acquire the pool using the generated config.
    pool <- Pool.acquire poolConfig

    -- --- Read SDEK credentials from environment variables ---
    -- getEnv reads a String from an env var. It will crash if the var is not set.
    sdekClientId <- fmap pack $ getEnv "SDEK_CLIENT_ID"
    sdekClientSecret <- fmap pack $ getEnv "SDEK_CLIENT_SECRET"
    sdekUrl <- fmap pack $ getEnv "SDEK_URL"

    -- 6. Create the shared AppState
    let appConfig = Config
          { _appDBPool = pool
          , _appLogEnv = logEnv
          , _providers = providers_
          , _sdekCred  = SdekCreds {..}
          , _sdekUrl   = sdekUrl 
          }

    -- 7. Run the server
    let port = configApiPort config -- <-- From config
    putStrLn $ "Starting server on port " <> show port
     -- 8. run server and clean up resources on shutdown
    app <- runApp appConfig
    run port app `finally` (Pool.release pool >> closeScribes logEnv)