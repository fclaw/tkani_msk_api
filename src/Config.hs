-- src/Config.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE TypeApplications  #-}

module Config
  ( Config(..)
  , loadConfig
  , maskSecrets
  ) where


import Data.Aeson
import Data.Either (isLeft)
import Control.Applicative ((<|>))
import Data.Text (Text, pack, unpack, strip)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isNothing)
import Control.Monad (join, when)
import qualified Data.Map as Map
import Control.Exception (catch, IOException)
import Database.PostgreSQL.Simple (ConnectInfo (..), defaultConnectInfo)
import Data.Int (Int64, Int32)
import Text.Printf (printf)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as LBS


import Text (textToInt, textToDouble, textToBool)
import Domain.Warehouse.Enums (FabricLifecycle)


instance ToJSON ConnectInfo
 

-- | A data type to hold all our application's configuration.
data Config = Config
  { configDBConnString           :: Text
  , configApiPort                :: Int
  , configConnInfo               :: ConnectInfo
  , configSdekClientId           :: Text
  , configSdekClientSecret       :: Text
  , configOrderBotToken          :: Text
  , configConciergeBotToken      :: Text
  , configWarehouseBotToken      :: Text
  , configConciergeChatId        :: Int64
  , configWarehouseChatId        :: Int64
  , configMainChatId             :: Int64
  , configOrderChatId            :: Int64
  , configYamlOrderChatId        :: Int64
  , configThresholdMetres        :: Double
  , configTinkoffTerminalKey     :: Text
  , configTinkoffSecret          :: Text
  , configTinkoffUrl             :: Text
  , configDailyDigestImgStub     :: Text
  , configCollageServiceUrl      :: Text
  , configCutTolerance           :: Int
  , configGalleryLink            :: Text
  , configIsCollageServiceOn     :: Bool
  , configCollageStubPath        :: Text
  , configMessageCannotBeDeleted :: Text
  , configMessageNotFound        :: Text
  , configIsCourierNeeded        :: Bool
  , configIsSpecialCourierNeeded :: Bool
  , configCourierWeightThreshold :: Int
  , configDostavistaToken        :: Text
  , configCourierCallCutoffHour  :: Maybe Int
  , configDostavistaUrl          :: Maybe Text
  , configGeocodeApiKey          :: Text
  , configGeocodeUrl             :: Text
  , configRateLimitAllowedUsers  :: [Int64]
  , configPostLifeDetails        :: [(FabricLifecycle, (Int, Int))]
  , configConciergeBotUrl        :: Text
  , configShelfChatId            :: Int64
  , configShelfCapacity          :: Int32
  , configTotalShelves           :: Int32
  , configPickupChatId           :: Int64
  , configSpecialPostChatId      :: Int64
  , configYandexApiUrl           :: Maybe Text
  , configYandexApiKey           :: Maybe Text
  , configPickupConsolidationTm  :: Int32
  , configPrepaidOrderChatId     :: Int64
  , configPdfCrowdUser           :: Text
  , configPdfCrowdApiKey         :: Text
  , configTinkoffOpenApiUrl      :: Text
  , configBankAccount            :: Text
  , configMoneyTransferChatId    :: Int64
  , configMoneyTransferToken     :: Text
  } deriving (Generic, ToJSON)

type EnvMap = Map.Map Text Text

-- | Parses a single line from a .env file (e.g., "KEY=VALUE")
parseEnvLine :: Text -> Maybe (Text, Text)
parseEnvLine line =
  case T.breakOn "=" (strip line) of
    (key, val) | not (T.null val) -> Just (strip key, strip (T.drop 1 val))
    _                             -> Nothing

-- | Reads a .env file and parses it into a Map.
loadEnvFile :: FilePath -> IO EnvMap
loadEnvFile path = 
  ( Map.fromList 
  . mapMaybe parseEnvLine
  . T.lines <$> TIO.readFile path)
  `catch` handleIOError
  where
    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
                          Just y  -> y : mapMaybe f xs
                          Nothing -> mapMaybe f xs
    handleIOError :: IOException -> IO EnvMap
    handleIOError _ = fmap (const Map.empty) $ putStrLn $ "Warning: .env file not found at " <> path <> ". Using defaults."

-- | Looks up a value in the EnvMap, returning a default if not found.
lookupWithDefault :: EnvMap -> Text -> Text -> Text
lookupWithDefault envMap key defValue = Map.findWithDefault defValue key envMap

extractNumber loc = fromMaybe (error ("cannot read number from text: " <> loc))


-- | The main function to load all configuration.
loadConfig :: IO Config
loadConfig = do
  -- 1. Load the .env file into a Map
  env <- loadEnvFile ".env"

  print $ "env --> " <> show env

  -- 2. Look up each variable from the Map, providing defaults
  let dbHost = lookupWithDefault env "POSTGRES_HOST" "localhost"
  let dbPort = lookupWithDefault env "POSTGRES_PORT" "5432"
  let dbUser = lookupWithDefault env "POSTGRES_USER" "youruser"
  let dbPass = lookupWithDefault env "POSTGRES_PASSWORD" "yourpass"
  let dbName = lookupWithDefault env "POSTGRES_DB" "tkani_db"
  let apiPortStr = lookupWithDefault env "API_INTERNAL_PORT" "8080"

  let configSdekClientId = (Map.!) env "SDEK_CLIENT_ID"
  let configSdekClientSecret = (Map.!) env "SDEK_CLIENT_SECRET"
  let configOrderBotToken = (Map.!) env "ORDER_BOT_TOKEN"
  let configConciergeBotToken = (Map.!) env "CONCIERGE_BOT_TOKEN"
  let configWarehouseBotToken = (Map.!) env "WAREHOUSE_BOT_TOKEN"
  let configConciergeChatId = fromIntegral $ extractNumber "CONCIERGE_CHAT_ID" $ textToInt $ (Map.!) env "CONCIERGE_CHAT_ID"
  let configPickupChatId = fromIntegral $ extractNumber "PICKUP_CHAT_ID" $ textToInt $ (Map.!) env "PICKUP_CHAT_ID"


  let configWarehouseChatId = fromIntegral $ extractNumber "WAREHOUSE_CHANNEL_ID" $ textToInt $ (Map.!) env "WAREHOUSE_CHANNEL_ID"
  let configMainChatId = fromIntegral $ extractNumber "MAIN_CHANNEL_ID" $ textToInt $ (Map.!) env "MAIN_CHANNEL_ID"
  let configOrderChatId = fromIntegral $ extractNumber "ORDER_CHAT_ID" $ textToInt $ (Map.!) env "ORDER_CHAT_ID"
  let configYamlOrderChatId = fromIntegral $ extractNumber "YAML_ORDER_CHAT_ID" $ textToInt $ (Map.!) env "YAML_ORDER_CHAT_ID"
  let configThresholdMetres = extractNumber "METRES_THRESHOLD" $ textToDouble $ (Map.!) env "METRES_THRESHOLD"

  let configShelfChatId = fromIntegral $ extractNumber "SHELF_CHAT_ID" $ textToInt $ (Map.!) env "SHELF_CHAT_ID"

  let configSpecialPostChatId = fromIntegral $ extractNumber "SPECIAL_POST_CHAT_ID" $ textToInt $ (Map.!) env "SPECIAL_POST_CHAT_ID"

  let configTinkoffTerminalKey = (Map.!) env "TINKOFF_TERMINAL_KEY"
  let configTinkoffSecret = (Map.!) env "TINKOFF_SECRET"
  let configTinkoffUrl = (Map.!) env "TINKOFF_URL"

  let configDailyDigestImgStub = (Map.!) env "DAILY_DIGEST_IMG_STUB"
  let configCollageServiceUrl = (Map.!) env "COLLAGE_SERVICE_URL"
  let configCutTolerance =  fromIntegral $ extractNumber "CUT_TOLERANCE" $ textToInt $ (Map.!) env "CUT_TOLERANCE"
  let configGalleryLink = (Map.!) env "GALLERY_LINK"
  let configIsCollageServiceOn = textToBool $ (Map.!) env "IS_COLLAGE_SERVICE_ON"
  let configCollageStubPath = (Map.!) env "COLLAGE_STUB_PATH"

  -- telegram error messages 
  let configMessageCannotBeDeleted = (Map.!) env "MESSAGE_CANNOT_BE_DELETED"
  let configMessageNotFound = (Map.!) env "MESSAGE_NOT_FOUND"
  
   -- courier
  let configIsCourierNeeded = textToBool $ (Map.!) env "IS_COURIER_NEEDED"
  let configIsSpecialCourierNeeded = textToBool $ (Map.!) env "IS_SPECIAL_COURIER_NEEDED"
  let configCourierWeightThreshold = fromIntegral $ extractNumber "COURIER_WEIGHT_THRESHOLD" $ textToInt $ (Map.!) env "COURIER_WEIGHT_THRESHOLD"
  
  let configCourierCallCutoffHour = fmap (fromIntegral . extractNumber "COURIER_CALL_CUTOFF_HOUR" . textToInt) $ (Map.!?) env "COURIER_CALL_CUTOFF_HOUR"
  let configDostavistaUrl = (Map.!?) env "DOSTAVISTA_URL"
  let configDostavistaToken = (Map.!) env "DOSTAVISTA_TOKEN"

  let configGeocodeApiKey = (Map.!) env "GEOCODE_API_KEY"
  let configGeocodeUrl = (Map.!) env "GEOCODE_URL"

  let configConciergeBotUrl = (Map.!) env "CONCIERGE_BOT_URL"

  let configRateLimitAllowedUsersMaybe = decode @[Int64] . LBS.pack . T.unpack $ (Map.!) env "RATE_LIMIT_ALLOWED_USER_IDS"

  when(isNothing configRateLimitAllowedUsersMaybe) $ error "cannot parse RATE_LIMIT_ALLOWED_USER_IDS"

  let configPostLifeDetailEither = eitherDecode @[(FabricLifecycle, (Int, Int))] . LBS.pack . T.unpack $ (Map.!) env "POST_LIFE_DETAILS"

  when(isLeft configPostLifeDetailEither) $ error $ "cannot parse POST_LIFE_DETAILS " <> show configPostLifeDetailEither

  let Right configPostLifeDetails = configPostLifeDetailEither
  let Just configRateLimitAllowedUsers = configRateLimitAllowedUsersMaybe

  -- 3. Parse the port number
  let configApiPort = fromMaybe 8080 (readMaybe $ unpack apiPortStr)

  let configYandexApiUrl = (Map.!?) env "YANDEX_API_URL"
  let configYandexApiKey = (Map.!?) env "YANDEX_API_KEY"

  -- 4. Construct the database connection string
  let configDBConnString =
        "host=" <> dbHost <>
        " port=" <> dbPort <>
        " user=" <> dbUser <>
        " password=" <> dbPass <>
        " dbname=" <> dbName

  let configConnInfo =
        defaultConnectInfo 
        { connectHost = T.unpack dbHost
        , connectPort = fromIntegral (read (T.unpack dbPort))
        , connectUser = T.unpack dbUser
        , connectPassword = T.unpack dbPass
        , connectDatabase = T.unpack dbName
        }

  let configShelfCapacity = fromIntegral $ extractNumber "SHELF_CAPACITY" $ textToInt $ (Map.!) env "SHELF_CAPACITY"

  let configTotalShelves = fromIntegral $ extractNumber "TOTAL_SHELVES" $ textToInt $ (Map.!) env "TOTAL_SHELVES"

  let configPickupConsolidationTm = maybe 16 (fromIntegral . extractNumber "CONSOLIDATION_TIME" . textToInt) $ (Map.!?) env "CONSOLIDATION_TIME" 

  let configPrepaidOrderChatId = fromIntegral $ extractNumber "PREPAID_ORDER_CHAT_ID" $ textToInt $ (Map.!) env "PREPAID_ORDER_CHAT_ID"

  let configPdfCrowdUser = (Map.!) env "PDF_CROWD_USER"
  let configPdfCrowdApiKey = (Map.!) env "PDF_CROWD_API_KEY"
 
  let configTinkoffOpenApiUrl = (Map.!) env "TINKOFF_OPENAPI_URL"

  let configBankAccount = (Map.!) env "BANK_ACCOUNT"

  let configMoneyTransferChatId = fromIntegral $ extractNumber "MONEY_TRANSFER_CHAT_ID" $ textToInt $ (Map.!) env "MONEY_TRANSFER_CHAT_ID"

  let configMoneyTransferToken = (Map.!) env "MONEY_TRANSFER_TOKEN"

  -- 5. Return the final Config record
  pure $ Config {..}


-- A helper to mask secrets for safe logging
maskSecret :: Text -> Text
maskSecret secret = 
    if T.length secret > 4
    then T.take 4 secret <> "..."
    else "..."

maskSecrets :: Config -> Config
maskSecrets config = 
  config 
  { configSdekClientSecret = 
      maskSecret (configSdekClientSecret config)
  , configOrderBotToken = 
      maskSecret (configOrderBotToken config)
  , configConciergeBotToken = 
      maskSecret (configOrderBotToken config)
  , configWarehouseBotToken = 
      maskSecret (configOrderBotToken config)
  , configTinkoffSecret = 
      maskSecret (configOrderBotToken config)
  }