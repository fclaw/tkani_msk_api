{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant (Handler)
import Servant.Server
import Servant.Server.Generic
-- Database and logging imports
import qualified Hasql.Pool as Pool
import Katip (initLogEnv, closeScribes)
import Control.Monad.Reader (runReaderT)
import qualified Hasql.Pool.Config as Config
import Hasql.Connection.Setting (connection)
import Hasql.Connection.Setting.Connection (string)
import Control.Monad (void)
import Control.Exception (finally)
import Network.Wai.Middleware.Cors (simpleCors) -- Import the middleware
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import GHC.IO.Exception (userError)
import Control.Monad.Error.Class (throwError)


import Types (AppState(..), AppM(..))
import Handlers (apiHandlers) -- Import our top-level record of handlers
import Config (loadConfig, AppConfig(..))

import API.Types (ProviderInfo)





handleProviderResult (Right providers) go = go providers
handleProviderResult (Left error) _ = throwError $ userError ("cannot open providers.yaml: " <> prettyPrintParseException error)


-- | This is the "natural transformation" that converts our 'AppM' into a 'Handler'.
--   It takes the 'AppState' and provides it to the 'ReaderT' within 'AppM'.
nt :: AppState -> AppM a -> Handler a
nt state appM = runReaderT (unAppM appM) state

-- | We use 'hoistServer' to apply our natural transformation to the whole server.
app :: AppState -> Application
app state = simpleCors $ genericServeT (nt state) apiHandlers


main :: IO ()
main = do
  -- 1. Load configuration from environment variables
  config <- loadConfig

  -- 2. Initialize the logger
  logEnv <- initLogEnv "tkani-api" "production"

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

  providerResult <- decodeFileEither @[ProviderInfo] "providers.yaml"
  handleProviderResult providerResult $ \providers_ -> do

    -- 6. Create the shared AppState
    let appState = AppState
          { appDBPool = pool
          , appLogEnv = logEnv
          , providers = providers_
          }

    -- 7. Run the server
    let port = configApiPort config -- <-- From config
    putStrLn $ "Starting server on port " <> show port
     -- 8. run server and clean up resources on shutdown
    run port (app appState) `finally` (Pool.release pool >> closeScribes logEnv)