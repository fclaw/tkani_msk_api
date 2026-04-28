{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main (main) where


import Katip
import Network.Wai.Handler.Warp (run)
import Servant (Handler)
import Data.List(sort)
import Servant.Server
import Data.Password.Bcrypt (PasswordHash, Bcrypt)
import Servant.Server.Generic
import Data.Maybe (fromMaybe)
import Servant.API.Generic (toServant)
-- Database and logging imports
import qualified Hasql.Pool as Pool
import qualified Data.Text as T
import Data.Bifunctor (first)
import Control.Monad.State.Class (get)
import Control.Monad.Reader (runReaderT)
import qualified Hasql.Pool.Config as Config
import Hasql.Connection.Setting (connection)
import Hasql.Connection.Setting.Connection (string)
import Control.Monad (void, when, forever, join)
import Control.Exception (finally, bracket, SomeException, throwIO, throwTo)
import Network.Wai.Middleware.Cors (simpleCors) -- Import the middleware
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import GHC.IO.Exception (userError)
import Control.Monad.Error.Class (throwError)
import System.Environment (getArgs, getEnv)
import Data.Text (pack)
import Data.Traversable (for)
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
import Control.Concurrent.Async.Lifted (async, waitAnyCatch, cancel, Async (..), waitCatch, AsyncCancelled (..))
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
import System.Timeout (timeout)


import Auth (verifyAdmin, AdminUser, HashedAdminPassword (..))
import qualified Lib.Servant.RateLimit as RL
import Handlers (apiHandlers) -- Import our top-level record of handlers
import qualified Config as GlobalCfg (loadConfig, Config(..), maskSecrets)
import API.Types (ProviderInfo)
import App (AppM(..), TinkoffCredentials (..), Config (..), State (..), MetroCity (..), runAppM, ChatKey (..), CityCodeByPVZCache (..), modifyTVarIO)
import API (tkaniApiProxy)
import Infrastructure.Logging.Telegram (mkTelegramScribe, getTelegramConfig)
import Infrastructure.Templating (loadTemplatesFromDirectory)
import Domain.Services.Warehouse (ensureWarehousePlatformId)
-- workers START
import Workers.SdekOrderStatusPoller (runSdekOrderStatusPoller)
import Workers.TinkoffPaymentStatusPoller (runTinkoffPaymentStatusPoller)
import Workers.CourierPickUpScheduler (runCourierPickUpScheduler)
import Workers.SdekStatusPoller (runSdekStatusPoller)
import Workers.PriceCalculator (runPriceCalculator)
import Workers.SdekGenerateReceipt (runSdekGenerateReceipt)
import Workers.OrderDeliveryScheduler (runOrderDeliveryScheduler)
import Workers.DailyWeightTracker (runDailyWeightTracker)
import Workers.DostavistaOrderStatusPoller (runDostavistaOrderStatusPoller)
import Workers.SpecialPostManager (runSpecialPostManager)
import Workers.OrderCancellationHandler (runOrderCancellationHandler)
import Workers.FabricLifecycleObserver (runFabricLifecycleObserver)
import Workers.DailyCleanupNotificationsJanitor (runDailyCleanupNotificationsJanitor)
import Workers.ShelfSubmissionObserver (runShelfSubmissionObserver)
import Workers.SdekCourierStatusPoller (runSdekCourierStatusPoller)
import Workers.SdekPickupAppStatusPoller (runSdekPickupAppStatusPoller)
import Workers.CancelledOrdersCleaner (runCancelledOrdersCleaner)
import Workers.ParcelDeliveryWatcher (runParcelDeliveryWatcher)
import Workers.DeliveryCostListener (runDeliveryCostListener)
import Workers.SimpleOrderOrchestrator (runSimpleOrderOrchestrator)
import Workers.ShelfOrderRegister (runShelfOrderRegister)
import Workers.YandexPickupStatusPoller (runYandexPickupStatusPoller)
import Workers.YandexOrderStatusPoller (runYandexOrderStatusPoller)
import Workers.YandexShipmentJanitor (runYandexShipmentJanitor)
import Workers.ShippingInvoiceJanitor (runShippingInvoiceJanitor)
import Workers.TinkoffShipmentPaymentStatusPoller (runTinkoffShipmentPaymentStatusPoller)
import Workers.YandexPrepaidOrderRegistrar (runYandexPrepaidOrderRegistrar)
import Workers.StuckOrdersWatcher (runStuckOrdersWatcher)
import Workers.ConsignmentNoteWatcher (runConsignmentNoteWatcher)
import Workers.YandexCatchupJanitor (runYandexCatchupJanitor)
import Workers.ExtraDiscountPromotionScheduler (runExtraDiscountPromotionScheduler)
import Workers.FabricLifecycleManager (runFabricLifecycleManager)
import Workers.InventoryStagnationJanitor (runInventoryStagnationJanitor)
import Workers.MoneyTransferStatusPoller  (runMoneyTransferStatusPoller)
-- workers END
import qualified Infrastructure.Services.Tinkoff.Manager as Tinkoff (setupManager)
import Infrastructure.Services.Overpass (fetchAllRussianMetros)
import Application.Cart (runCartsCleaner)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Sdek.Types.Config (SdekConfig(..), SdekCredentials (..))
import Infrastructure.Services.Yandex.Config (YandexConfig, apiUrl, apiKey)
import Infrastructure.Services.Dostavista.Types.Config (DostavistaConfig (..))
import qualified Infrastructure.Services.Dostavista.Types.Config as Dostativsta
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)



data Workers = 
        WebServer 
      | SdekOrderStatusPoller 
      | Tinkoff 
      | CollageMaker 
      | CartsCleaner
      | CourierPickUpScheduler
      | SdekStatusPoller
      | PriceCalculator
      | SdekGenerateReceipt
      | OrderDeliveryScheduler
      | DailyWeightTracker
      | DostavistaOrderStatusPoller
      | SpecialPostManager
      | OrderCancellationHandler
      | FabricLifecycleObserver
      | DailyCleanupNotificationsJanitor
      | ShelfSubmissionObserver
      | SdekCourierStatusPoller
      | SdekPickupAppStatusPoller
      | CancelledOrdersCleaner
      | ParcelDeliveryWatcher
      | DeliveryCostListener
      | SimpleOrderOrchestrator
      | ShelfOrderRegister
      | YandexOrderStatusPoller
      | YandexPickupStatusPoller
      | YandexShipmentJanitor
      | ShippingInvoiceJanitor
      | TinkoffShipmentPaymentStatusPoller
      | YandexPrepaidOrderRegistrar
      | StuckOrdersWatcher
      | ConsignmentNoteWatcher
      | YandexCatchupJanitor
      | ExtraDiscountPromotionScheduler
      | FabricLifecycleManager
      | InventoryStagnationJanitor
      | MoneyTransferStatusPoller




instance Show Workers where
  show WebServer                          = "Web Server"
  show SdekOrderStatusPoller              = "SDEK Order Status Poller"
  show Tinkoff                            = "Tinkoff Poller"
  show CollageMaker                       = "Collage Maker"
  show CartsCleaner                       = "Carts Cleaner"
  show CourierPickUpScheduler             = "Courier Pickup Scheduler"
  show SdekStatusPoller                   = "SDEK Status Poller"
  show PriceCalculator                    = "Price Calculator"
  show SdekGenerateReceipt                = "SDEK Generate Receipt"
  show OrderDeliveryScheduler             = "Order Delivery Scheduler"
  show DailyWeightTracker                 = "Daily Weight Tracker"
  show DostavistaOrderStatusPoller        = "Dostavista Order Status Poller"
  show SpecialPostManager                 = "Special Post Manager"
  show OrderCancellationHandler           = "Order Cancellation Handler"
  show FabricLifecycleObserver            = "Fabric Lifecycle Observer"
  show DailyCleanupNotificationsJanitor   = "Daily Cleanup Notifications Janitor"
  show ShelfSubmissionObserver            = "Shelf Submission Observer"
  show SdekCourierStatusPoller            = "SDEK Courier Status Poller"
  show SdekPickupAppStatusPoller          = "SDEK Pickup App Status Poller"
  show CancelledOrdersCleaner             = "Cancelled Orders Cleaner"
  show ParcelDeliveryWatcher              = "Parcel Delivery Watcher"
  show DeliveryCostListener               = "Delivery Cost Listener"
  show SimpleOrderOrchestrator            = "Simple Order Orchestrator"
  show ShelfOrderRegister                 = "Shelf Order Register"
  show YandexOrderStatusPoller            = "Yandex Order Status Poller"
  show YandexPickupStatusPoller           = "Yandex Pickup Status Poller"
  show YandexShipmentJanitor              = "Yandex Shipment Janitor"
  show ShippingInvoiceJanitor             = "Shipping Invoice Janitor"
  show TinkoffShipmentPaymentStatusPoller = "Tinkoff Shipment Payment Status Poller"
  show YandexPrepaidOrderRegistrar        = "Yandex Prepaid Order Registrar"
  show StuckOrdersWatcher                 = "Stuck Orders. Watcher"
  show ConsignmentNoteWatcher             = "Consignment Note Watcher"
  show YandexCatchupJanitor               = "Yandex Catchup Janitor"
  show ExtraDiscountPromotionScheduler    = "Extra Discount Promotion Scheduler"
  show FabricLifecycleManager             = "Fabric Lifecycle Manager"
  show InventoryStagnationJanitor         = "Inventory Stagnation Janitor"
  show MoneyTransferStatusPoller          = "Money Transfer Status Poller"

--


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
    eYandexConfig <- decodeFileEither @YandexConfig "config/yandex.yaml"
    eDostavistaConfig <- decodeFileEither @DostavistaConfig "config/dostavista.yaml"
    let res = (,,,,) <$> eProviders <*> eMetroCities <*> eSdekConfig <*> eYandexConfig <*> eDostavistaConfig
    handleYamlResult res $ \(providers, cities, sdekConfig, yandexConfig, dostavistaConfig) -> do
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

      let tinkoffTerminalKey        = configTinkoffTerminalKey
      let tinkoffSecret             = configTinkoffSecret
      let tinkoffUrl                = configTinkoffUrl
      let tinkoffOpenApiUrl         = configTinkoffOpenApiUrl
      let tinkoffMoneyTransferToken = configMoneyTransferToken

      -- 6. Setup Tinkoff Open API manager
      tinkoffCert    <- getEnv "TBANK_CERT_PATH" -- Should return "/run/secrets/tbank_cert" in Docker
      tinkoffKey     <- getEnv "TBANK_KEY_PATH"
      openApiManager <- Tinkoff.setupManager (T.unpack configTinkoffOpenApiUrl) tinkoffCert tinkoffKey

      -- 7. Create the shared AppState
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
            , _yandexConfig =
               yandexConfig
               { apiUrl = fromMaybe (apiUrl yandexConfig) configYandexApiUrl
               , apiKey = fromMaybe (apiKey yandexConfig) configYandexApiKey
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
                   (SPECIAL_POST, (configConciergeBotToken, configSpecialPostChatId)),
                   (PREPAID_ORDER, (configConciergeBotToken, configPrepaidOrderChatId)),
                   (MONEY_TRANSFER, (configConciergeBotToken, configMoneyTransferChatId)),
                   (SERVER_SHUTDOWN, (configConciergeBotToken, configServerShutdownChatId))
                   ]
            , _configHttpManager = tlsManager
            , _tinkoffOpenApiManager = openApiManager
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
            , _consolidationTime = configPickupConsolidationTm
            , _pdfCrowdUser = configPdfCrowdUser
            , _pdfCrowdApiKey = configPdfCrowdApiKey
            , _bankAccount = configBankAccount
            , _adminUser = configAdminUser
            , _adminPassHash = getBcryptHash $ read @HashedAdminPassword $ T.unpack configAdminPassHash
            , _pdfServiceUrl = configPdfServiceUrl
            }

      tinkoffPaymentChan  <- newTChanIO
      sdekOrderChan       <- newTChanIO
      sdekCourierChan     <- newTChanIO
      simpleOrdersChan    <- newTChanIO
      shelfOrdersChan     <- newTChanIO
      dostavistaChan      <- newTChanIO
      shipmentChan        <- newTChanIO
      tinkoffShipmentChan <- newTChanIO

      cityCacheVar <- newTVarIO M.empty
      pvzCacheVar  <- newTVarIO M.empty

      let state =
           State 
           { _sdekToken           = Nothing
           , _pointCache          = mempty
           , _sdekPromises        = mempty
           , _tinkoffPaymentChan  = tinkoffPaymentChan
           , _sdekOrderChan       = sdekOrderChan
           , _sdekCourierChan     = sdekCourierChan
           , _metroStations       = []
           , _cityCodeByPVZCache  = CityCodeByPVZCache {..}
           , _dostavistaChan      = dostavistaChan
           , _allSdekPointsCache  =  Nothing
           , _sdekTariffs         = mempty
           , _sdekPointsCodes     = Nothing
           , _simpleOrdersChan    = simpleOrdersChan
           , _shelfOrdersChan     = shelfOrdersChan
           , _yandexPickupPoints  = mempty
           , _yandexDropOffPoints = Nothing
           , _yandexWarehouseId   = Nothing
           , _shipmentChan        = shipmentChan
           , _tinkoffShipmentChan = tinkoffShipmentChan
           }
      initialState <- newTVarIO state
  
      -- Create the runner function that bridges AppM and IO.
      let appMToHandler :: forall a. AppM a -> IO (Either ServerError a)
          appMToHandler = runAppM appConfig initialState

      -- fetch Yandex warehouse platform station id
      eWarehouse <- 
        fmap (first (T.pack . show)) $
          appMToHandler $
            if configIsSpecialCourierNeeded then do
              eRes <- ensureWarehousePlatformId
              for eRes $ \platformId -> do
                stateTVar <- get
                modifyTVarIO stateTVar $ \s -> 
                  s { _yandexWarehouseId = 
                      Just platformId }
            else pure $ Right ()

      case join $  eWarehouse of 
        Right _ -> pure ()
        Left e  -> error $ T.unpack e

      when(not isMetroMode) $
        putStrLn "--> Running in NO-METRO mode. Metro data will not be loaded."

      eAllMetros <- if not isMetroMode then return (Right []) else appMToHandler fetchAllRussianMetros
      for_ eAllMetros $ \allMetros -> do
        liftIO $ atomically $ modifyTVar' initialState $
          \s -> s { _metroStations = allMetros }

        -- Define our concurrent tasks as a list of IO actions.
        -- Task 1: The Web Server
        rateLimitState <- newTVarIO @RL.State (RL.State M.empty configRateLimitAllowedUsers)
        let context = verifyAdmin appConfig :. rateLimitState :. EmptyContext
        let server =
              run configApiPort $ 
                methodsCors $  
                  serveWithContext 
                  tkaniApiProxy
                  context $
                    hoistServerWithContext
                      tkaniApiProxy
                      (Proxy @'[BasicAuthCheck AdminUser, RL.RateLimitState])
                      (appToHandler appConfig initialState) 
                      (toServant apiHandlers)

        let connInfo = configConnInfo
        let tasks :: [(Workers, IO ())]
            tasks = 
              [ (WebServer, server)
              , (SdekOrderStatusPoller, 
                 runForever 60 $
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
              , (PriceCalculator,
                  appMToHandler (
                    runPriceCalculator 
                    connInfo 
                    appMToHandler)
                   >>= showErrorInWorker
                        PriceCalculator)
              , (SdekGenerateReceipt, 
                  appMToHandler (
                    runSdekGenerateReceipt 
                    connInfo 
                    appMToHandler)
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
                 runForever 10 $
                   appMToHandler (runSpecialPostManager)
                     >>= showErrorInWorker
                          SpecialPostManager)
              , (OrderCancellationHandler,
                 appMToHandler (
                   runOrderCancellationHandler 
                   connInfo 
                   appMToHandler)
                   >>= showErrorInWorker
                        OrderCancellationHandler)
              , (FabricLifecycleObserver,
                 appMToHandler (
                   runFabricLifecycleObserver 
                   connInfo 
                   appMToHandler)
                   >>= showErrorInWorker
                        FabricLifecycleObserver)
              , (DailyCleanupNotificationsJanitor,
                 runForever 720 $ -- twice a day
                   appMToHandler (runDailyCleanupNotificationsJanitor)
                     >>= showErrorInWorker
                           DailyCleanupNotificationsJanitor)
              , (ShelfSubmissionObserver,
                 appMToHandler (
                   runShelfSubmissionObserver 
                   connInfo 
                   appMToHandler)
                   >>= showErrorInWorker
                        ShelfSubmissionObserver)
              , (CancelledOrdersCleaner,
                 runForever 30 $
                   appMToHandler (runCancelledOrdersCleaner)
                     >>= showErrorInWorker
                            CancelledOrdersCleaner)
              , (ParcelDeliveryWatcher,
                 runForever 720 $
                   appMToHandler (runParcelDeliveryWatcher)
                     >>= showErrorInWorker
                           ParcelDeliveryWatcher)
              , (DeliveryCostListener,
                   appMToHandler (
                     runDeliveryCostListener 
                     connInfo 
                     appMToHandler)
                     >>= showErrorInWorker
                           DeliveryCostListener)
              , (SimpleOrderOrchestrator,
                   appMToHandler
                     runSimpleOrderOrchestrator
                     >>= showErrorInWorker
                           SimpleOrderOrchestrator)
              , (ShelfOrderRegister,
                   appMToHandler
                     runShelfOrderRegister
                     >>= showErrorInWorker
                           ShelfOrderRegister)
              , (YandexOrderStatusPoller,
                 runForever 60 $
                   appMToHandler
                   runYandexOrderStatusPoller
                    >>= showErrorInWorker
                          YandexOrderStatusPoller)
              , (ShippingInvoiceJanitor,
                   appMToHandler
                     runShippingInvoiceJanitor 
                     >>= showErrorInWorker
                           ShippingInvoiceJanitor)
              , (TinkoffShipmentPaymentStatusPoller,
                  appMToHandler
                    runTinkoffShipmentPaymentStatusPoller
                    >>= showErrorInWorker
                          TinkoffShipmentPaymentStatusPoller)
              , (YandexPrepaidOrderRegistrar,
                  appMToHandler
                    (runYandexPrepaidOrderRegistrar 
                     connInfo 
                     appMToHandler)
                    >>= showErrorInWorker
                          YandexPrepaidOrderRegistrar)
              , (StuckOrdersWatcher,
                 runForever 5 $
                  appMToHandler
                    runStuckOrdersWatcher
                    >>= showErrorInWorker
                          StuckOrdersWatcher)
              , (ConsignmentNoteWatcher,
                  appMToHandler
                    (runConsignmentNoteWatcher 
                     connInfo 
                     appMToHandler)
                    >>= showErrorInWorker
                          ConsignmentNoteWatcher)
              , (ExtraDiscountPromotionScheduler,
                  runForever 5 $
                    appMToHandler
                      runExtraDiscountPromotionScheduler
                      >>= showErrorInWorker
                            ExtraDiscountPromotionScheduler)
              , (FabricLifecycleManager,
                 runForever 720 $
                  appMToHandler runFabricLifecycleManager
                    >>= showErrorInWorker
                          FabricLifecycleManager)
              , (InventoryStagnationJanitor,
                 runForever 720 $ -- twice a day
                  appMToHandler runInventoryStagnationJanitor
                    >>= showErrorInWorker
                          InventoryStagnationJanitor)
              , (MoneyTransferStatusPoller,
                 runForever 60 $ 
                  appMToHandler runMoneyTransferStatusPoller
                    >>= showErrorInWorker
                          MoneyTransferStatusPoller)
              ]

        let courierPickupTasks 
              | configIsCourierNeeded =
                let weightTrackerWorker =
                     (DailyWeightTracker,
                      appMToHandler (
                        runDailyWeightTracker 
                        connInfo 
                        appMToHandler)
                        >>= showErrorInWorker
                             DailyWeightTracker)
                    dostavistaWorker =
                     (DostavistaOrderStatusPoller,
                      appMToHandler runDostavistaOrderStatusPoller
                        >>= showErrorInWorker 
                             DostavistaOrderStatusPoller)         
                in [weightTrackerWorker, dostavistaWorker]
              | configIsSpecialCourierNeeded =
                let courierWorker =
                     (CourierPickUpScheduler, do
                      -- Initialize the lock variable
                      lastRunVar <- newTVarIO Nothing
                      runForever 10 $
                        appMToHandler 
                        (runCourierPickUpScheduler 
                         lastRunVar)
                          >>= showErrorInWorker
                            CourierPickUpScheduler)
                    sdekCourierStatusPoller =
                     (SdekCourierStatusPoller,
                      appMToHandler 
                      (runSdekCourierStatusPoller)
                        >>= showErrorInWorker
                          SdekCourierStatusPoller)
                    sdekPickupAppStatusPoller =
                     (SdekPickupAppStatusPoller,
                      runForever 5 $
                        appMToHandler 
                        (runSdekPickupAppStatusPoller)
                          >>= showErrorInWorker
                            SdekPickupAppStatusPoller)
                    yandexPickupStatusPoller =
                     (YandexPickupStatusPoller,
                      runForever 5 $
                        appMToHandler 
                        (runYandexPickupStatusPoller)
                          >>= showErrorInWorker
                            YandexPickupStatusPoller)
                    yandexShipmentJanitor =
                     (YandexShipmentJanitor,
                      runForever 5 $
                        appMToHandler 
                        (runYandexShipmentJanitor)
                          >>= showErrorInWorker
                            YandexShipmentJanitor)
                    yandexCatchupJanitor =
                     (YandexCatchupJanitor, do
                      lastRunVar <- newTVarIO Nothing
                      runForever 10 $
                        appMToHandler 
                        (runYandexCatchupJanitor 
                         lastRunVar)
                          >>= showErrorInWorker
                            YandexCatchupJanitor)
                in [ courierWorker
                   , sdekCourierStatusPoller
                   , yandexShipmentJanitor
                   , sdekPickupAppStatusPoller
                   , yandexPickupStatusPoller
                   , yandexCatchupJanitor
                   ]
              | otherwise = []

        putStrLn "Spawning concurrent workers..."
        asyncs <- mapM (\(name, action) -> (show name,) <$> async action) $ tasks <> courierPickupTasks
        putStrLn "All workers started. Waiting for any worker to exit."

        -- Supervise the tasks. 'waitAny' will block and re-throw any exception.
        (taskName, _) <- waitAnyNamed asyncs
        
        -- Gracefully cancel all other workers on exit.
        -- 1. Trigger cancellations (You already have this)
        --    SIGNAL everyone to die (NON-BLOCKING)
        for_ asyncs $ \(name, a) ->
          when (name /= taskName) $ do
            putStrLn $ "📣 Sending stop signal to: " <> name
            -- 'throwTo' sends the exception and moves on immediately
            liftIO $ throwTo (asyncThreadId a) AsyncCancelled

        -- 2. Wait with a GLOBAL timeout for all of them
        putStrLn "⏳ Waiting for workers to clean up (max 5s)..."
        res <- liftIO $ timeout 5000000 $ mapM_ (void . waitCatch . snd) asyncs
              
        case res of
          Nothing -> putStrLn "⚠️ Some workers failed to exit in time. Forcing shutdown."
          Just _  -> putStrLn "✅ All workers terminated gracefully."

        appMToHandler $ do
           $(logTM) AlertS $ logStr $ "Worker '" <> T.pack taskName <> "' finished. Initiating shutdown sequence."
           let msg = "Worker '" <> T.pack taskName <> "' finished unexpectedly. System has been shut down."
           void $ sendOrEditTelegramMessage mempty (escapeMarkdownV2 msg) SERVER_SHUTDOWN Nothing Nothing Nothing
           -- Perform any additional cleanup if necessary (e.g., close DB connections, flush logs)
           liftIO $ threadDelay 1000000 -- Give some time for logs to flush 

        throwIO $ userError $ "Worker '" <> taskName <> "' finished unexpectedly. System has been shut down."