{-# LANGUAGE OverloadedStrings #-}

module Utils.CollageMaker (generateCollageViaPython, downloadAndSave) where

import System.Process (callProcess)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BL
import Network.Wreq (get, responseBody, Response)
import Control.Lens ((^.))
import Data.Text (Text)
import qualified Data.Text as T
import UnliftIO.Async (pooledMapConcurrentlyN)
import Control.Exception (try, SomeException)
import Data.Word (Word32)
import Data.Foldable (for_)
import System.Environment (getExecutablePath)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)



-- | Orchestrates the Python call.
--   Returns the FilePath to the generated collage image.
generateCollageViaPython :: [Text] -> Word32 -> IO (Maybe FilePath)
generateCollageViaPython urls jobId = do
    let tempDir = "/tmp/collage_job_" <> show jobId
    let outputImg = tempDir <> "_result.jpg"

    -- Read the script path from an environment variable.
    -- Default to the Docker path if the variable is not set.
    maybeScriptPath <- lookupEnv "COLLAGE_SCRIPT_PATH"
    let scriptPath = fromMaybe "/app/utils/make_grid.py" maybeScriptPath

    -- --- END OF NEW LOGIC ---

    createDirectoryIfMissing True tempDir

    -- 2. Download Images to Temp Directory
    putStrLn $ "Downloading " <> show (length urls) <> " images..."
    _ <- pooledMapConcurrentlyN 5 (downloadAndSave tempDir) (zip [1..] urls)

    -- 3. Call Python Script
    -- Usage: python collage_maker.py -f FOLDER -o OUTPUT -w WIDTH -c COLS
    putStrLn "Running Python script..."
    result <- try $ callProcess "python" 
        [ scriptPath
        , "-f", tempDir      -- Folder containing images
        , "-o", outputImg    -- Output filename
        , "-w", "1200"       -- Width
        ] :: IO (Either SomeException ())

    -- 4. Cleanup and Return
    case result of
        Left err -> do
            putStrLn $ "Python script failed: " <> show err
            removeDirectoryRecursive tempDir -- Cleanup input files
            return Nothing
        
        Right _ -> do
            exists <- doesFileExist outputImg
            removeDirectoryRecursive tempDir -- Cleanup input files
            
            if exists 
                then return (Just outputImg)
                else return Nothing

-- | Helper to download a single URL and save to a file
downloadAndSave :: FilePath -> (Int, Text) -> IO ()
downloadAndSave dir (index, url) = do
    let filename = dir </> "img_" <> show index <> ".jpg"
    eResp <- try $ get (T.unpack url) :: IO (Either SomeException (Network.Wreq.Response BL.ByteString))
    for_ eResp $ \r -> BL.writeFile filename (r ^. responseBody)