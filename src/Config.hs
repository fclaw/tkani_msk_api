-- src/Config.hs

{-# LANGUAGE OverloadedStrings #-}

module Config
  ( AppConfig(..)
  , loadConfig
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text, pack, unpack, strip)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Exception (catch, IOException)

-- | A data type to hold all our application's configuration.
data AppConfig = AppConfig
  { configDBConnString :: Text
  , configApiPort      :: Int
  } deriving (Show)

type EnvMap = Map.Map Text Text

-- | Parses a single line from a .env file (e.g., "KEY=VALUE")
parseEnvLine :: Text -> Maybe (Text, Text)
parseEnvLine line =
  case T.breakOn "=" (strip line) of
    (key, val) | not (T.null val) -> Just (strip key, strip (T.drop 1 val))
    _                             -> Nothing

-- | Reads a .env file and parses it into a Map.
loadEnvFile :: FilePath -> IO EnvMap
loadEnvFile path = (Map.fromList . mapMaybe parseEnvLine . T.lines <$> TIO.readFile path)
  `catch` handleIOError
  where
    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
                          Just y  -> y : mapMaybe f xs
                          Nothing -> mapMaybe f xs
    handleIOError :: IOException -> IO EnvMap
    handleIOError _ = do
      putStrLn $ "Warning: .env file not found at " ++ path ++ ". Using defaults."
      return Map.empty

-- | Looks up a value in the EnvMap, returning a default if not found.
lookupWithDefault :: EnvMap -> Text -> Text -> Text
lookupWithDefault envMap key defValue = Map.findWithDefault defValue key envMap

-- | The main function to load all configuration.
loadConfig :: IO AppConfig
loadConfig = do
  -- 1. Load the .env file into a Map
  env <- loadEnvFile ".env"

  -- 2. Look up each variable from the Map, providing defaults
  let dbHost = lookupWithDefault env "POSTGRES_HOST" "localhost"
  let dbPort = lookupWithDefault env "POSTGRES_PORT" "5432"
  let dbUser = lookupWithDefault env "POSTGRES_USER" "youruser"
  let dbPass = lookupWithDefault env "POSTGRES_PASSWORD" "yourpass"
  let dbName = lookupWithDefault env "POSTGRES_DB" "tkani_db"
  let apiPortStr = lookupWithDefault env "API_INTERNAL_PORT" "8080"
  
  -- 3. Parse the port number
  let apiPort = fromMaybe 8080 (readMaybe $ unpack apiPortStr)

  -- 4. Construct the database connection string
  let connString =
        "host=" <> dbHost <>
        " port=" <> dbPort <>
        " user=" <> dbUser <>
        " password=" <> dbPass <>
        " dbname=" <> dbName

  -- 5. Return the final AppConfig record
  pure $ AppConfig
    { configDBConnString = connString
    , configApiPort      = apiPort
    }