{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Utils.CollageMaker (generateCollageViaService, downloadImage) where

import Katip
import Data.Aeson.TH
import Data.UUID (toText)
import qualified Data.ByteString.Lazy as BL
import Control.Lens ((^.), (.~), (&))
import Data.Text (Text)
import qualified Data.Text as T
import UnliftIO.Async (pooledMapConcurrentlyN)
import Control.Exception (try, SomeException)
import Data.Word (Word32)
import Data.Foldable (for_)
import Control.Monad.Reader.Class (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Network.HTTP.Client (Manager)
import Data.UUID.V4 (nextRandom)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Network.Wreq (getWith, defaults, manager, responseBody, Response)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)


import Text (camelToSnake)
import Infrastructure.Utils.Http (postReq)
import App (AppM, _configHttpManager, _collageServiceUrl, _isCollageServiceOn, _collageStubPath)


-- Define the request and response ADTs
data CollageRequest = CollageRequest
  { imagePaths     :: [Text]
  , outputFilename :: Text
  , width          :: Int
  }

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake } ''CollageRequest)

data CollageResponse = CollageResponse
  { ok          :: Bool
  , resultPath  :: Maybe Text
  , error       :: Maybe Text
  }

$(deriveJSON defaultOptions { fieldLabelModifier = camelToSnake } ''CollageResponse)

showt :: Show a => a -> T.Text
showt = T.pack . show

generateCollageViaService :: [Text] -> Word32 -> AppM (Either Text FilePath)
generateCollageViaService urls jobId = do
  
    cfg <- ask

    let isOn = _isCollageServiceOn cfg
    let stubPath = _collageStubPath cfg
    let mgr = _configHttpManager cfg
    let collageServiceUrl = T.unpack $ _collageServiceUrl cfg
    
    if not isOn then
      liftIO $ fmap Right $ getStubFilePath mgr stubPath
    else do
      -- 1. Define paths within the shared volume (from the API container's perspective)
      sharedVolumePath <- liftIO $ fromMaybe "/data"  <$> lookupEnv "SHARED_DATA_PATH" -- The mount point in the tkani-api container
      let jobDir = sharedVolumePath </> ("collage_job_" <> show jobId)
      let outputFilename = "collage_result_" <> showt jobId <> ".jpg"

      -- Create a temporary directory for the downloaded images
      liftIO $ createDirectoryIfMissing True jobDir

      -- 2. Download all images from Telegram concurrently
      $(logTM) InfoS $ ls $ "Downloading " <> showt (length urls) <> " images for job " <> showt jobId
      -- 'downloadImage' is a helper that takes a file_id and saves it to a path
      -- It returns the final path. We need to handle potential errors.
      let mgr = _configHttpManager cfg
      eDownloadedPaths <- liftIO $ try $ pooledMapConcurrentlyN 5 (downloadImage mgr jobDir) (zip [1..] urls)

      case eDownloadedPaths of
        Left (dlErr :: SomeException) -> do
          liftIO $ removeDirectoryRecursive jobDir -- Cleanup on failure
          return $ Left ("Failed to download images: " <> showt dlErr)

        Right downloadedPaths -> do
          -- 3. Create the request for the Python service
          -- We send paths *relative* to the shared volume root.
          let relativePaths = map (makeRelative sharedVolumePath) downloadedPaths
              
          let requestPayload = 
               CollageRequest
               { imagePaths = relativePaths
               , outputFilename = outputFilename
               , width = 1200
               }

          $(logTM) InfoS "Calling collage service..."
          eResult <- postReq @CollageResponse mgr (collageServiceUrl <> "/generate-collage") requestPayload [] Nothing

          -- 4. Clean up the temporary input images immediately
          liftIO $ removeDirectoryRecursive jobDir
              
          -- 5. Process the response from the collage service
          case eResult of
            Left httpErr -> 
              return $ Left ("Collage service connection failed: " <> showt httpErr)
            Right (CollageResponse isOk maybeRelativePath maybeError) ->
              if isOk then do 
                let Just relativePath = maybeRelativePath
                return $ Right (sharedVolumePath </> T.unpack relativePath)
              else do
                let Just error = maybeError
                return $ Left ("Collage service error: " <> error)
                    
-- | Helper to download a single URL and save to a file
downloadImage :: Manager -> FilePath -> (Int, Text) -> IO FilePath
downloadImage mgr dir (index, url) = do
  let filename = dir </> "img_" <> show index <> ".jpg"
  let baseOpts = defaults & manager .~ Right mgr
  eResp <- try $ getWith baseOpts (T.unpack url) :: IO (Either SomeException (Network.Wreq.Response BL.ByteString))
  fmap (const filename) $ for_ eResp $ \r -> BL.writeFile filename (r ^. responseBody)

-- | Strips the shared volume's base path to create a relative path.
--   Example: makeRelative "/app/tmp" "/app/tmp/job_123/1.jpg" -> "job_123/1.jpg"
makeRelative :: FilePath -> FilePath -> Text
makeRelative basePath fullPath =
  let
    baseText = T.pack basePath
    fullText = T.pack fullPath
  in
    -- T.stripPrefix returns a Maybe, so we handle the case where the prefix
    -- doesn't match (which would indicate a bug).
    fromMaybe fullText (T.stripPrefix (baseText <> "/") fullText)


getStubFilePath :: Manager -> Text -> IO FilePath
getStubFilePath mgr url = do
   -- 1. Create a unique filename
  uuid <- nextRandom
  timestamp <- round `fmap` getPOSIXTime :: IO Int
  tempDir <- getCurrentDirectory -- Use the system's temp directory
  let filename = tempDir </> (show timestamp <> "_" <> T.unpack (toText uuid) <> ".jpg")
  let baseOpts = defaults & manager .~ Right mgr
  eResp <- try $ getWith baseOpts (T.unpack url) :: IO (Either SomeException (Network.Wreq.Response BL.ByteString))
  fmap (const filename) $ for_ eResp $ \r -> BL.writeFile filename (r ^. responseBody)