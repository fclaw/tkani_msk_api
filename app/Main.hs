{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
import Control.Exception (finally, bracket, SomeException)
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
import Control.Monad.Catch (catch, throwM)
import Data.Text (Text)
import Data.List (find)
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Concurrent.Async.Lifted (async, waitAnyCatch, cancel, Async (..))
import qualified Data.HashSet as HS

import Handlers (apiHandlers) -- Import our top-level record of handlers
import Config (loadConfig, AppConfig(..))
import API.Types (ProviderInfo)
import App (AppM(..), SDEKCredentials (..), Config (..), State (..), MetroCity (..), runAppM)
import API (tkaniApiProxy)
import Infrastructure.Logging.Telegram (mkTelegramScribe, getTelegramConfig)
import Infrastructure.Templating (loadTemplatesFromDirectory)
import Workers.SdekOrderStatusPoller (sdekOrderStatusPoller)


handleYamlResult (Right providers) go = go providers
handleYamlResult (Left error) _ = throwError $ userError ("cannot open yaml: " <> prettyPrintParseException error)

whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) f = f x
whenLeft _        _ = pure ()

-- This is the "natural transformation" that converts our 'AppM' into a 'Handler'
appToHandler :: forall a . Config -> TVar State -> AppM a -> Handler a
appToHandler config stateTVar appM = do
  -- Run the RWST computation
  -- It gives us the result 'a', the final state 's', and the writer output 'w'
  -- Define the handler for a catastrophic, unhandled exception
  let exceptionHandler (e :: SomeException) = do
       -- This is where we log the specific error
       $(logTM) EmergencyS $ logStr $ "FATAL: Unhandled exception reached the top-level handler: " <> show e
       throwM e
  let runMonadsStack = runRWST (unAppM (appM `catch` exceptionHandler)) config stateTVar
  let selRes (a, _, _) = a
  result <- liftIO $ runExceptT $ fmap selRes runMonadsStack
  -- Handle the result of the ExceptT
  whenLeft result throwError -- Propagate Servant errors
  let Right unwrap = result 
  return unwrap


-- | A helper function to set up and tear down the Katip LogEnv
withLogEnv :: Manager -> (LogEnv -> IO a) -> IO a
withLogEnv tlsManager action = do
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
      telegramScribe <- mkTelegramScribe tlsManager config InfoS V2
      -- Register it using the default settings.
      pure $ logEnvWithStdout >>= registerScribe "telegram" telegramScribe defaultScribeSettings
  
  let finalLogEnv = logEnvWithTelegram

  -- 5. Use 'bracket' to ensure scribes are closed properly.
  bracket finalLogEnv closeScribes action


-- | A helper that waits for any of a list of named async tasks to finish.
--   It correctly captures both successful results and exceptions.
waitAnyNamed :: [(String, Async a)] -> IO (String, Either SomeException a)
waitAnyNamed namedAsyncs = do
  -- Use waitAnyCatch, which is designed for this exact purpose.
  -- Its signature is: [Async a] -> IO (Async a, Either SomeException a)
  (finishedAsync, eitherResult) <- waitAnyCatch (map snd namedAsyncs)
  -- Find the name associated with the finished async handle
  case find (\(_, a) -> asyncThreadId a == asyncThreadId finishedAsync) namedAsyncs of
    Just (name, _) ->
      -- Return the found name and the 'Either' result directly.
      pure (name, eitherResult)
    Nothing ->
      -- This fallback should ideally never be reached.
      pure ("<unknown>", eitherResult)


main :: IO ()
main = do
  -- Step 1: Create a new TLS-enabled manager using our custom settings.
  -- This is where the magic from 'http-client-tls' happens.
  tlsManager <- newManager tlsManagerSettings
  withLogEnv tlsManager $ \logEnv -> do
    eProviders <- decodeFileEither @[ProviderInfo] "providers.yaml"
    eMetroCities <- decodeFileEither @[MetroCity] "data/metro_cities.yaml"
    let res = (,) <$> eProviders <*> eMetroCities
    handleYamlResult res $ \(providers, cities) -> do
      tplMap <- loadTemplatesFromDirectory "templates"

      -- 2. Load configuration from environment variables
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
      sdekTariffCode <- fmap (read @Int) $ getEnv "SDEK_TARIFF_CODE" 
      sdekShipmentPoint <- fmap pack $ getEnv "SDEK_SHIPMENT_POINT" 
      orderBotToken <- fmap pack $ getEnv "ORDER_BOT_TOKEN"
      orderChatId <- fmap pack $ getEnv "ORDER_CHAT_ID"
      yandexApiKey <- pack <$> getEnv "YANDEX_API_KEY"

      -- 6. Create the shared AppState
      let appConfig = Config
            { _appDBPool = pool
            , _appLogEnv = logEnv
            , _providers = providers
            , _sdekCred  = SdekCreds {..}
            , _sdekUrl   = sdekUrl
            , _sdekTariffCode = sdekTariffCode
            , _sdekShipmentPoint = sdekShipmentPoint
            , _configBotToken = orderBotToken
            , _orderChatId = orderChatId
            , _configHttpManager = tlsManager
            , configTemplateMap = tplMap
            , _configYandexApiKey = yandexApiKey
            , _metroCityCodes = HS.fromList (map code cities)
            }

      let state = 
           State 
           { _sdekToken = Nothing
           , _pointCache = mempty
           , _sdekPromises = mempty 
           }
      initialState <- newTVarIO state

      -- Create the runner function that bridges AppM and IO.
      let runInIO :: forall a. AppM a -> IO (Either ServerError a)
          runInIO = runAppM appConfig initialState
      -- Define our concurrent tasks as a list of IO actions.
      -- Task 1: The Web Server
      let server = 
           run (configApiPort config) $ 
             simpleCors $  
               serve tkaniApiProxy $
                 hoistServer 
                   tkaniApiProxy 
                   (appToHandler appConfig initialState) 
                   (toServant apiHandlers)
      -- Task 2: The SDEK Polling Worker
      let sdekPoller = do
            res <- runInIO sdekOrderStatusPoller 
            whenLeft res $ \e ->
              error $ "SDEK Poller failed with a servant error: " ++ show e
      let tasks :: [(String, IO ())]
          tasks = [("Web Server", server), ( "SDEK Poller", sdekPoller)]

      putStrLn "Spawning concurrent workers..."
      asyncs <- mapM (\(name, action) -> (name,) <$> async action) tasks
      putStrLn "All workers started. Waiting for any worker to exit."

      -- Supervise the tasks. 'waitAny' will block and re-throw any exception.
      (taskName, _) <- waitAnyNamed asyncs
      putStrLn $ "Worker '" ++ taskName ++ "' finished unexpectedly. Shutting down."

      -- Gracefully cancel all other workers on exit.
      mapM_ (cancel . snd) asyncs
      putStrLn "Shutdown complete."