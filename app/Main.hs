{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where


import Katip
import Network.Wai.Handler.Warp (run)
import Servant (Handler)
import Data.List(sort)
import Servant.Server
import Servant.Server.Generic
import Data.Maybe (fromMaybe)
import Servant.API.Generic (toServant)
-- Database and logging imports
import qualified Hasql.Pool as Pool
import Control.Monad.Reader (runReaderT)
import qualified Hasql.Pool.Config as Config
import Hasql.Connection.Setting (connection)
import Hasql.Connection.Setting.Connection (string)
import Control.Monad (void, when, forever)
import Control.Exception (finally, bracket, SomeException)
import Network.Wai.Middleware.Cors (simpleCors) -- Import the middleware
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import GHC.IO.Exception (userError)
import Control.Monad.Error.Class (throwError)
import System.Environment (getArgs)
import Data.Text (pack)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, newTChanIO, modifyTVar')
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
import qualified Data.Map.Strict as M
import Data.Foldable (for_)
import Network.Wai.Middleware.Cors
import Network.Wai (Middleware)
import Network.HTTP.Types.Method (StdMethod(DELETE, PUT, PATCH), renderStdMethod)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent (threadDelay)


import qualified Lib.Servant.RateLimit as RL
import Handlers (apiHandlers) -- Import our top-level record of handlers
import qualified Config as GlobalCfg (loadConfig, Config(..), maskSecrets)
import API.Types (ProviderInfo)
import App (AppM(..), TinkoffCredentials (..), Config (..), State (..), MetroCity (..), runAppM, ChatKey (..), CityCodeByPVZCache (..))
import API (tkaniApiProxy)
import Infrastructure.Logging.Telegram (mkTelegramScribe, getTelegramConfig)
import Infrastructure.Templating (loadTemplatesFromDirectory)
-- workers START
import Workers.SdekOrderStatusPoller (runSdekOrderStatusPoller)
import Workers.TinkoffPaymentStatusPoller (runTinkoffPaymentStatusPoller)
import Workers.SdekPickUpScheduler (runSdekPickUpScheduler)
import Workers.SdekStatusPoller (runSdekStatusPoller)
import Workers.SdekPriceCalculator (runSdekPriceCalculator)
import Workers.SdekGenerateReceipt (runSdekGenerateReceipt)
import Workers.OrderDeliveryScheduler (runOrderDeliveryScheduler)
import Workers.DailyWeightTracker (runDailyWeightTracker)
import Workers.DostavistaOrderStatusPoller (runDostavistaOrderStatusPoller)
import Workers.SpecialPostManager (runSpecialPostManager)
import Workers.SdekOrderCancellationHandler (runSdekOrderCancellationHandler)
import Workers.FabricLifecycleObserver (runFabricLifecycleObserver)
import Workers.DailyCleanupNotificationsJanitor (runDailyCleanupNotificationsJanitor)
import Workers.ShelfSubmissionObserver (runShelfSubmissionObserver)
import Workers.SdekCourierStatusPoller (runSdekCourierStatusPoller)
import Workers.SdekPickupAppStatusPoller (runSdekPickupAppStatusPoller)
-- workers END
import Infrastructure.Services.Overpass (fetchAllRussianMetros)
import Application.Cart (runCartsCleaner)
import Infrastructure.Services.Sdek.Types.Config (SdekConfig(..), SdekCredentials (..))
import Infrastructure.Services.Dostavista.Types.Config (DostavistaConfig (..))
import qualified Infrastructure.Services.Dostavista.Types.Config as Dostativsta



data Workers = 
        WebServer 
      | SdekOrderStatusPoller 
      | Tinkoff 
      | CollageMaker 
      | CartsCleaner
      | SdekPickUpScheduler
      | SdekStatusPoller
      | SdekPriceCalculator
      | SdekGenerateReceipt
      | OrderDeliveryScheduler
      | DailyWeightTracker
      | DostavistaOrderStatusPoller
      | SpecialPostManager
      | SdekOrderCancellationHandler
      | FabricLifecycleObserver
      | DailyCleanupNotificationsJanitor
      | ShelfSubmissionObserver
      | SdekCourierStatusPoller
      | SdekPickupAppStatusPoller

instance Show Workers where
  show WebServer                        = "Web Server"
  show SdekOrderStatusPoller            = "SDEK Order Status Poller"
  show Tinkoff                          = "Tinkoff Poller"
  show CollageMaker                     = "Collage Maker"
  show CartsCleaner                     = "Carts Cleaner"
  show SdekPickUpScheduler              = "SDEK Pickup Scheduler"
  show SdekStatusPoller                 = "SDEK Status Poller"
  show SdekPriceCalculator              = "SDEK Price Calculator"
  show SdekGenerateReceipt              = "SDEK Generate Receipt"
  show OrderDeliveryScheduler           = "Order Delivery Scheduler"
  show DailyWeightTracker               = "Daily Weight Tracker"
  show DostavistaOrderStatusPoller      = "Dostavista Order Status Poller"
  show SpecialPostManager               = "Special Post Manager"
  show SdekOrderCancellationHandler     = "SDEK Order Cancellation Handler"
  show FabricLifecycleObserver          = "Fabric Lifecycle Observer"
  show DailyCleanupNotificationsJanitor = "Daily Cleanup Notifications Janitor"
  show ShelfSubmissionObserver          = "Shelf Submission Observer"
  show SdekCourierStatusPoller          = "SDEK Courier Status Poller"
  show SdekPickupAppStatusPoller        = "SDEK Pickup App Status Poller"



methodsCors :: Middleware
methodsCors = cors $ const (Just (simpleCorsResourcePolicy { corsMethods = map renderStdMethod [ DELETE, PUT, PATCH]}))

handleYamlResult (Right providers) go = go providers
handleYamlResult (Left error) _ = throwError $ userError ("cannot open yaml: " <> prettyPrintParseException error)

whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) f = f x
whenLeft _        _ = pure ()

showErrorInWorker worker res = whenLeft res $ \e -> error $ show worker <> " failed with a servant error: " <> show e

runForever delay worker = forever $ do worker; threadDelay (delay * 60 * 1000000)


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
  args <- getArgs
  let isMetroMode = not ("no-metro" `elem` args)

  -- Step 1: Create a new TLS-enabled manager using our custom settings.
  -- This is where the magic from 'http-client-tls' happens.
  tlsManager <- newManager tlsManagerSettings
  withLogEnv tlsManager $ \logEnv -> do
    eProviders <- decodeFileEither @[ProviderInfo] "providers.yaml"
    eMetroCities <- decodeFileEither @[MetroCity] "data/metro_cities.yaml"
    eSdekConfig <- decodeFileEither @SdekConfig "config/sdek.yaml"
    eDostavistaConfig <- decodeFileEither @DostavistaConfig "config/dostavista.yaml"
    let res = (,,,) <$> eProviders <*> eMetroCities <*> eSdekConfig <*> eDostavistaConfig
    handleYamlResult res $ \(providers, cities, sdekConfig, dostavistaConfig) -> do
      tplMap <- loadTemplatesFromDirectory "templates"

      -- 2. Load configuration from environment variables
      cfg@GlobalCfg.Config {..} <- GlobalCfg.loadConfig

      let maskSecretCfg = GlobalCfg.maskSecrets cfg
      TIO.putStrLn $ "global config has been loaded: \n" <> decodeUtf8 (BL.toStrict (encodePretty maskSecretCfg))

      -- 3. Define the connection string.
      let connString = string configDBConnString
        
      -- 4. Build the configuration using the DSL.
      let poolConfig = Config.settings
            [ Config.size 10                         -- Pool size of 10
            , Config.acquisitionTimeout 10           -- Timeout of 10 seconds
            , Config.staticConnectionSettings [connection connString] -- The connection string itself
            ]

      -- 5. Acquire the pool using the generated config.
      pool <- Pool.acquire poolConfig

      let tinkoffTerminalKey = configTinkoffTerminalKey
      let tinkoffSecret      = configTinkoffSecret
      let tinkoffUrl         = configTinkoffUrl

      -- 6. Create the shared AppState
      let appConfig = Config
            { _appDBPool = pool
            , _appLogEnv = logEnv
            , _providers = providers
            , _tinkoffCred = TinkoffCredentials {..}
            , _sdekConfig = 
                 sdekConfig
                 { credentials = 
                   SdekCredentials 
                   configSdekClientId 
                   configSdekClientSecret
                 , tariffs = sort (tariffs sdekConfig)
                 }
            , _bots =
                M.fromList
                  [(CONCIERGE, (configConciergeBotToken, configConciergeChatId)),
                   (ORDER, (configOrderBotToken, configOrderChatId)),
                   (WAREHOUSE, (configWarehouseBotToken, configWarehouseChatId)),
                   (MAIN, (configConciergeBotToken, configMainChatId)),
                   (YAML_ORDER, (configWarehouseBotToken, configYamlOrderChatId)),
                   (SHELF, (configConciergeBotToken, configShelfChatId)),
                   (PICKUP, (configConciergeBotToken, configPickupChatId)),
                   (SPECIAL_POST, (configConciergeBotToken, configSpecialPostChatId))
                   ]
            , _configHttpManager = tlsManager
            , configTemplateMap = tplMap
            , _metroCityCodes = HS.fromList (map code cities)
            , _thresholdMetres = configThresholdMetres
            , _dailyDigestImgStub = configDailyDigestImgStub
            , _collageServiceUrl = configCollageServiceUrl
            , _cutTolerance = configCutTolerance
            , _galleryLink = configGalleryLink
            , _isCollageServiceOn = configIsCollageServiceOn
            , _collageStubPath = configCollageStubPath
            , _messageCannotBeDeleted = configMessageCannotBeDeleted
            , _messageNotFound = configMessageNotFound
            , _courierWeightThreshold = configCourierWeightThreshold
            , _dostavistaConfig = 
               dostavistaConfig 
               { token = configDostavistaToken
              , courierCallCutoffHour = 
                fromMaybe 
                (courierCallCutoffHour dostavistaConfig)
                configCourierCallCutoffHour
              , url = fromMaybe (Dostativsta.url dostavistaConfig) configDostavistaUrl
               }
            , _geocodeApiKey = configGeocodeApiKey
            , _geocodeUrl = configGeocodeUrl
            , _postsCfgs = configPostLifeDetails
            , _conciergeBotUrl = configConciergeBotUrl
            , _shelfCapacity = configShelfCapacity
            , _totalShelves = configTotalShelves
            }

      tinkoffPaymentChan <- newTChanIO
      sdekOrderChan <- newTChanIO
      sdekCourierChan <- newTChanIO

      cityCacheVar <- newTVarIO M.empty
      pvzCacheVar <- newTVarIO M.empty

      dostavistaChan <- newTChanIO

      let state =
           State 
           { _sdekToken          = Nothing
           , _pointCache         = mempty
           , _sdekPromises       = mempty
           , _tinkoffPaymentChan = tinkoffPaymentChan
           , _sdekOrderChan      = sdekOrderChan
           , _sdekCourierChan    = sdekCourierChan
           , _metroStations      = []
           , _cityCodeByPVZCache = CityCodeByPVZCache {..}
           , _dostavistaChan     = dostavistaChan
           , _allSdekPointsCache =  Nothing
           , _sdekTariffs        = mempty
           }
      initialState <- newTVarIO state
  
      -- Create the runner function that bridges AppM and IO.
      let appMToHandler :: forall a. AppM a -> IO (Either ServerError a)
          appMToHandler = runAppM appConfig initialState

      when(not isMetroMode) $
        putStrLn "--> Running in NO-METRO mode. Metro data will not be loaded."

      eAllMetros <- if not isMetroMode then return (Right []) else appMToHandler fetchAllRussianMetros
      for_ eAllMetros $ \allMetros -> do
        liftIO $ atomically $ modifyTVar' initialState $
          \s -> s { _metroStations = allMetros }

        -- Define our concurrent tasks as a list of IO actions.
        -- Task 1: The Web Server
        rateLimitState <- newTVarIO @RL.State (RL.State M.empty configRateLimitAllowedUsers)
        let context = rateLimitState :. EmptyContext
        let server =
              run configApiPort $ 
                methodsCors $  
                  serveWithContext 
                  tkaniApiProxy 
                  context $
                    hoistServerWithContext
                      tkaniApiProxy
                      (Proxy @'[RL.RateLimitState])
                      (appToHandler appConfig initialState) 
                      (toServant apiHandlers)

        let connInfo = configConnInfo
        let tasks :: [(Workers, IO ())]
            tasks = 
              [ (WebServer, server)
              , (SdekOrderStatusPoller, 
                 runForever 5 $
                   appMToHandler runSdekOrderStatusPoller
                     >>= showErrorInWorker 
                           SdekOrderStatusPoller)
              , (Tinkoff, 
                 appMToHandler runTinkoffPaymentStatusPoller 
                   >>= showErrorInWorker 
                        Tinkoff)
              , (CartsCleaner,
                 runForever 1 $
                   appMToHandler runCartsCleaner 
                     >>= showErrorInWorker 
                           CartsCleaner)
              , (SdekStatusPoller,
                 appMToHandler runSdekStatusPoller 
                   >>= showErrorInWorker 
                        SdekStatusPoller)
              , (SdekPriceCalculator,
                  appMToHandler (runSdekPriceCalculator connInfo appMToHandler)
                   >>= showErrorInWorker
                        SdekPriceCalculator)
              , (SdekGenerateReceipt, 
                  appMToHandler (runSdekGenerateReceipt connInfo appMToHandler)
                   >>= showErrorInWorker
                        SdekGenerateReceipt)
              , (OrderDeliveryScheduler, do
                 -- Initialize the lock variable
                 lastRunVar <- newTVarIO Nothing
                 runForever 10 $
                   appMToHandler (runOrderDeliveryScheduler lastRunVar)
                     >>= showErrorInWorker 
                           OrderDeliveryScheduler)
              , (SpecialPostManager,
                 appMToHandler (runSpecialPostManager)
                   >>= showErrorInWorker
                        SpecialPostManager)
              , (SdekOrderCancellationHandler,
                 appMToHandler (runSdekOrderCancellationHandler connInfo appMToHandler)
                   >>= showErrorInWorker
                        SdekOrderCancellationHandler)
              , (FabricLifecycleObserver,
                 appMToHandler (runFabricLifecycleObserver connInfo appMToHandler)
                   >>= showErrorInWorker
                        FabricLifecycleObserver)
              , (DailyCleanupNotificationsJanitor,
                 runForever 720 $ -- twice a day
                   appMToHandler (runDailyCleanupNotificationsJanitor)
                     >>= showErrorInWorker
                           DailyCleanupNotificationsJanitor)
              , (ShelfSubmissionObserver,
                 appMToHandler (runShelfSubmissionObserver connInfo appMToHandler)
                   >>= showErrorInWorker
                        ShelfSubmissionObserver)
              ]

        let courierPickupTasks 
              | configIsCourierNeeded =
                let weightTrackerWorker =
                     (DailyWeightTracker,
                      appMToHandler (runDailyWeightTracker connInfo appMToHandler)
                        >>= showErrorInWorker
                             DailyWeightTracker)
                    dostavistaWorker =
                     (DostavistaOrderStatusPoller,
                      appMToHandler runDostavistaOrderStatusPoller 
                        >>= showErrorInWorker 
                             DostavistaOrderStatusPoller)         
                in [weightTrackerWorker, dostavistaWorker]
              | configIsSdekCourierNeeded =
                let sdekCourierWorker =
                     (SdekPickUpScheduler, do
                      -- Initialize the lock variable
                      lastRunVar <- newTVarIO Nothing
                      runForever 10 $
                        appMToHandler (runSdekPickUpScheduler lastRunVar)
                          >>= showErrorInWorker 
                            SdekPickUpScheduler)
                    sdekCourierStatusPoller =
                     (SdekCourierStatusPoller,
                      appMToHandler (runSdekCourierStatusPoller)
                        >>= showErrorInWorker
                          SdekCourierStatusPoller)
                    sdekPickupAppStatusPoller =
                     (SdekPickupAppStatusPoller,
                      runForever 5 $
                        appMToHandler (runSdekPickupAppStatusPoller)
                          >>= showErrorInWorker
                            SdekPickupAppStatusPoller)
                in [sdekCourierWorker, sdekCourierStatusPoller, sdekPickupAppStatusPoller]
              | otherwise = []

        putStrLn "Spawning concurrent workers..."
        asyncs <- mapM (\(name, action) -> (show name,) <$> async action) $ tasks <> courierPickupTasks
        putStrLn "All workers started. Waiting for any worker to exit."

        -- Supervise the tasks. 'waitAny' will block and re-throw any exception.
        (taskName, _) <- waitAnyNamed asyncs
        putStrLn $ "Worker '" <> taskName <> "' finished unexpectedly. Shutting down."

        -- Gracefully cancel all other workers on exit.
        mapM_ (cancel . snd) asyncs >> putStrLn "Shutdown complete."