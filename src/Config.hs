module Config
  ( AppConfig(..)
  , loadConfig
  ) where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

-- | A data type to hold all our application's configuration.
data AppConfig = AppConfig
  { configDBConnString :: Text
  , configApiPort      :: Int
  } deriving (Show)

-- | A helper function to read an environment variable with a default value.
getEnvOrDefault :: String -> String -> IO String
getEnvOrDefault varName defValue = fromMaybe defValue <$> lookupEnv varName

-- | The main function to load all configuration from the environment.
loadConfig :: IO AppConfig
loadConfig = do
  dbHost <- getEnvOrDefault "POSTGRES_HOST" "localhost"
  dbPort <- getEnvOrDefault "POSTGRES_PORT" "5432"
  dbUser <- getEnvOrDefault "POSTGRES_USER" "youruser"
  dbPass <- getEnvOrDefault "POSTGRES_PASSWORD" "yourpass"
  dbName <- getEnvOrDefault "POSTGRES_DB" "tkani_db"
  
  -- Read the API port, with a default value of 8080
  apiPortStr <- getEnvOrDefault "API_INTERNAL_PORT" "8080"
  let apiPort = fromMaybe 8080 (readMaybe apiPortStr)

  -- Construct the database connection string
  let connString = pack $
        "host=" ++ dbHost ++
        " port=" ++ dbPort ++
        " user=" ++ dbUser ++
        " password=" ++ dbPass ++
        " dbname=" ++ dbName
        
  pure $ AppConfig
    { configDBConnString = connString
    , configApiPort      = apiPort
    }