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
{-# LANGUAGE TypeApplications           #-}

module App
  ( State(..),
    AppM(..),
    Config (..),
    SdekToken (..),
    MetroCity (..),
    ChatKey (..),
    TinkoffCredentials (..),
    Scheme (..),
    SdekJob (..),
    CityCodeByPVZCache (..),
    SdekPvzInfo (..),
    SdekCity (..),
    DostavistaJob (..),
    PaymentFlow (..),
    NormalizedRoute (..),
    SdekCourierJob (..),
    SdekPointCode (..),
    currentTime,
    render,
    runAppM,
    -- helper (TVAR, TChan)
    readTVarIO,
    modifyTVarIO,
    readTChanIO,
    writeTChanIO,
    -- extractors
    extractFromMaybe,
    extractFromEither,
    -- forkAppM
    forkAppM
  ) where



import Katip
import Control.Applicative (pure)
import Data.Monoid (mempty)
import Data.Int (Int64, Int32)
import Control.Monad (void)
import Servant (Handler, ServerError)
import Hasql.Pool (Pool)
import Data.Text (Text, pack)
import Control.Lens
import GHC.Generics (Generic)
import Data.Aeson.TH
import Control.Monad.Catch
import Control.Monad.Time
import Network.HTTP.Client (Manager)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS (runRWST)
import Data.Time (UTCTime)
import qualified Data.HashSet as HS
import Control.Concurrent (MVar)
import Data.UUID (UUID)
import Data.Char (toLower)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Monad.Except (MonadError, ExceptT)
import Language.Haskell.TH (loc_module, location)
import Data.Aeson (Value, FromJSON, parseJSON, withObject, (.:))
import Control.Monad.RWS (RWST (..), MonadState, withRWST) -- Important
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, local)
import Control.Concurrent.STM (TVar, TChan, readTVar, modifyTVar', atomically, readTChan, writeTChan)

import API.WithField (WithField)
import Text (recordLabelModifier, camelToSnake)
import API.Types (ProviderInfo, DeliveryPoint, OrderRequest, InitiateShelfShipment)
import Infrastructure.Templating (TemplateMap, renderTemplate, TemplateData)
import Domain.Warehouse.Enums (FabricLifecycle)
import Infrastructure.Services.Yandex.Config (YandexConfig)
import Infrastructure.Services.Sdek.Types.Geocode (SdekPoint)
import Infrastructure.Services.Sdek.Types.Courier (SdekPickupAppStatus)
import Infrastructure.Services.Sdek.Types (SdekConfirmation, SdekError, Location, mkLocation)
import Infrastructure.Services.Tinkoff.Types.GetState (GetStateRequest)
import Infrastructure.Services.Overpass.Types (MetroStation)
import Infrastructure.Services.Sdek.Types.Config (SdekConfig)
import Infrastructure.Services.Dostavista.Types.Config (DostavistaConfig)
import Infrastructure.Services.Dostavista.Types.Enums (DostavistaOrderStatus)
import Infrastructure.Services.Sdek.Types.State (SdekRequestState)
import Infrastructure.Services.Yandex.Types (GeoId, PlatformStationId, PickupPoint, DropOffPoint)


data Scheme = HTTP | HTTPS

instance Show Scheme where
  show HTTP  = "http://"
  show HTTPS = "https://"


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
 , sdekObtainedAt  :: UTCTime
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

-- The TMVar will hold the final result
type ReplyVar = TMVar (Either Text Text)

-- The job passed to the poller
data SdekJob = SdekJob
  { sjSdekUuid :: UUID
  , sjReplyVar :: ReplyVar -- The reply box
  }

type ReplyCourierVar = TMVar (Either Text (SdekRequestState, SdekPickupAppStatus))

data SdekCourierJob =
     SdekCourierJob
     { scjSdekUuid :: UUID
     , scjReplyVar :: ReplyCourierVar -- The reply box
     }

-- Minimal info needed for a PVZ
data SdekPvzInfo = SdekPvzInfo
  { spvzCode     :: Text
  , spvzCityName :: Text -- The key to look up in the City Cache
  }

-- Info for a City
data SdekCity = SdekCity
  { scCode :: Int
  , scName :: Text
  -- ... other city info
  }

data CityCodeByPVZCache = 
     CityCodeByPVZCache 
     { cityCacheVar :: TVar (M.Map Text SdekCity)
     , pvzCacheVar  :: TVar (M.Map Text SdekPvzInfo) 
     }

data DostavistaJob = 
     DostavistaJob 
     { doJobOrderId     :: Int64
     , doJobOrderStatus :: DostavistaOrderStatus
     , doJobAStart      :: UTCTime
     }


data PaymentFlow = PutOnShelf | ShipNow
  deriving (Show, Eq)


-- This newtype wraps our tuple. Its only purpose is to provide a custom Ord instance.
newtype NormalizedRoute = NormalizedRoute (Location, Location) deriving (Show)

-- A smart constructor for our route, which uses the location smart constructor.
mkNormalizedRoute :: Int -> Int -> NormalizedRoute
mkNormalizedRoute from to = NormalizedRoute (mkLocation from, mkLocation to)

-- This is the function that performs the directional normalization.
-- It ensures the "smaller" location always comes first in the tuple.
normalizeTuple :: (Location, Location) -> (Location, Location)
normalizeTuple (a, b) = if a <= b then (a, b) else (b, a)

-- --- The Magic: Custom Eq and Ord Instances ---

-- Two routes are equal if their *normalized* versions are equal.
instance Eq NormalizedRoute where
  (NormalizedRoute r1) == (NormalizedRoute r2) =
    normalizeTuple r1 == normalizeTuple r2

-- The ordering of two routes is based on the ordering of their *normalized* versions.
instance Ord NormalizedRoute where
  compare (NormalizedRoute r1) (NormalizedRoute r2) =
    compare (normalizeTuple r1) (normalizeTuple r2)


data SdekPointCode = 
     SdekPointCode
     { spcCode    :: Text
     , spcAddress :: Text
     } deriving Show

-- Helper ADT for the nested 'location' object, extracting only what's needed.
-- We can ignore most of its fields if only address_full is important.
data SdekPointLocation = SdekPointLocation
  { locAddressFull :: Text
  -- You could add locLatitude :: Scientific, locLongitude :: Scientific here if needed
  } deriving (Show, Generic)

-- Custom FromJSON instance for the nested location.
-- It's important that this maps 'address_full' to 'locAddressFull'.
instance FromJSON SdekPointLocation where
  parseJSON = withObject "SdekPointLocation" $ \v -> SdekPointLocation
    <$> v .: "address_full"

instance FromJSON SdekPointCode where
  parseJSON = 
    withObject "SdekPointCode" $ \o -> do
    -- Extract 'code' from the top-level object 'o'.
    code <- o .: "code"
    
    -- Extract the nested 'location' object from 'o'.
    locationObj <- o .: "location"
    
    -- Parse the 'locationObj' into our SdekPointLocation helper type.
    -- This uses the FromJSON instance for SdekPointLocation.
    parsedLocation <- parseJSON locationObj
    
    -- Now, construct our SdekPointCode record.
    pure $ SdekPointCode
      { spcCode    = code
      , spcAddress = locAddressFull parsedLocation -- Get the specific field from the parsed location.
      }


-- This will be our mutable, thread-safe state.
-- It holds the SDEK token and its expiry time.
data State = State
  { _sdekToken           :: Maybe SdekToken -- Stored in a TVar for thread safety
  , _pointCache          :: PointCache
  , _cityCodeByPVZCache  :: CityCodeByPVZCache
  , _sdekTariffs         :: M.Map NormalizedRoute (UTCTime, [Int])
  , _yandexPickupPoints  :: M.Map GeoId (UTCTime, [WithField "metros" [Text] PickupPoint])
  , _yandexDropOffPoints :: Maybe (UTCTime, [DropOffPoint])
  , _yandexWarehouseId   :: Maybe PlatformStationId
  , _sdekPromises        :: SdekPromiseMap
  , _tinkoffPaymentChan  :: TChan (PaymentFlow, Text, GetStateRequest)
  , _sdekOrderChan       :: TChan SdekJob
  , _sdekCourierChan     :: TChan SdekCourierJob
  , _metroStations       :: [MetroStation]
  , _dostavistaChan      :: TChan DostavistaJob
  , _allSdekPointsCache  :: Maybe (UTCTime, [SdekPoint]) -- ADD THIS LINE
  , _sdekPointsCodes     :: Maybe (UTCTime, [SdekPointCode])
  , _simpleOrdersChan    :: TChan OrderRequest
  , _shelfOrdersChan     :: TChan (Int64, WithField "chat_id" Int64 InitiateShelfShipment)
  }


data ChatKey = 
        ORDER 
      | CONCIERGE 
      | WAREHOUSE
      | MAIN
      | YAML_ORDER
      | SHELF
      | PICKUP
      | SPECIAL_POST
        deriving (Show, Ord, Eq)

type Bots = M.Map ChatKey (Text, Int64)


type PostCfg = (Int, Int)

-- | AppState holds all the shared, read-only resources for our application.
data Config = Config
  { _appDBPool              :: Pool
  , _appLogEnv              :: LogEnv
  , _providers              :: [ProviderInfo]
  , _tinkoffCred            :: TinkoffCredentials
  , _sdekConfig             :: SdekConfig
  , _yandexConfig           :: YandexConfig
  , _bots                   :: Bots
  , _configHttpManager      :: Manager
  , configTemplateMap       :: TemplateMap
  , _metroCityCodes         :: HS.HashSet Int
  , _thresholdMetres        :: Double -- Threshold: If stock falls below this, hide the fabric.
  , _dailyDigestImgStub     :: Text
  , _collageServiceUrl      :: Text
  , _cutTolerance           :: Int
  , _galleryLink            :: Text
  , _isCollageServiceOn     :: Bool
  , _collageStubPath        :: Text
  , _messageCannotBeDeleted :: Text
  , _messageNotFound        :: Text
  , _courierWeightThreshold :: Int
  , _dostavistaConfig       :: DostavistaConfig
  , _geocodeApiKey          :: Text
  , _geocodeUrl             :: Text
  , _postsCfgs              :: [(FabricLifecycle, PostCfg)]
  , _conciergeBotUrl        :: Text
  , _shelfCapacity          :: Int32
  , _totalShelves           :: Int32
  , _consolidationTime      :: Int32
  }

-- A helper type for parsing the YAML
newtype MetroCity = MetroCity { code :: Int }
instance FromJSON MetroCity where
  parseJSON = withObject "MetroCity" $ \v -> MetroCity <$> v .: "code"


data TinkoffCredentials =
     TinkoffCredentials 
     { tinkoffTerminalKey :: Text
     , tinkoffSecret      :: Text
     , tinkoffUrl         :: Text
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

readTVarIO = (liftIO @AppM) . atomically . readTVar
{-# INLINE readTVarIO #-}

modifyTVarIO var = (liftIO @AppM) . atomically . modifyTVar' var
{-# INLINE modifyTVarIO #-}

readTChanIO = (liftIO @AppM) . atomically . readTChan
{-# INLINE readTChanIO #-}

writeTChanIO chan = (liftIO @AppM) . atomically . writeTChan chan
{-# INLINE writeTChanIO #-}

extractFromMaybe :: Maybe a -> (a -> AppM ()) -> AppM ()
extractFromMaybe (Just v) app = app v 
extractFromMaybe Nothing _ = $(logTM) ErrorS $ "empty value"

extractFromEither :: Show e => Either e a -> (a -> AppM ()) -> AppM ()
extractFromEither (Right r) app = app r 
extractFromEither (Left e) _ = $(logTM) ErrorS $ ls $ "either has resulted in error: " <> show e


-- | Forks an AppM action into a separate thread. This is for "fire-and-forget"
--   tasks where we do not need the result. It ensures any exceptions in the
--   background thread are caught and logged, preventing them from crashing the server.
forkAppM :: AppM () -> AppM ()
forkAppM action = do
  -- We need the application's config/environment to run the action in the new thread.
  config <- ask
  state <- get
  liftIO $ void $ forkIO $ do
    -- 'try' will catch any synchronous exception.
    eResult <- try (runAppM config state action) -- 'runAppM' is your function to execute AppM in IO.
    case eResult of
      Right (Right _) -> pure () -- Success
      Right (Left serverErr) ->
        -- The AppM action resulted in a business logic error. Log it.
        putStrLn $ "Error in forked thread (AppM): " ++ show serverErr
      Left (ex :: SomeException) ->
        -- The IO action itself threw a raw exception. Log it.
        putStrLn $ "Exception in forked thread (IO): " ++ show ex

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''PaymentFlow)
$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''ChatKey)