{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE RankNTypes  #-}

module Application.Listener (runCollageJobListener, truncateFabricNames, cleanDigestText) where


import Katip
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void, when, join)
import Data.Int (Int64)
import Data.Aeson.TH
import Data.Aeson (eitherDecode, (.=), object)
import Control.Concurrent.Async as Async
import Data.Either (isLeft)
import Data.Foldable (for_)
import System.Random (randomIO)
import Data.Word (Word32)
import Control.Monad.Reader.Class (ask)
import qualified Data.Map.Strict as M
import Servant.Server (ServerError)
import System.Directory (removeFile)
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Bifunctor (first)
import Data.Maybe (isNothing)
import Control.Exception (try, SomeException)
import Network.Wreq (getWith, responseBody, Response, manager, defaults)
import Control.Lens ((^.), (.~), (&))
import Network.HTTP.Client (Manager)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)


import App (AppM, ChatKey(MAIN, WAREHOUSE), _appDBPool, _galleryLink, _isCollageServiceOn, _collageStubPath, _configHttpManager) -- Your AppM types
import API.Types (DailyDigest(DailyDigest)) 
import Text (recordLabelModifier) 
import Utils.CollageMaker (generateCollageViaService)
import Infrastructure.Services.Telegram (sendPhotoToTelegram, deleteMessage)
import TH.Location (currentModule)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Database (setDailyDigestStatus)
import Infrastructure.Database.Types (DailyDigestStatus (Published))


maxNamesToList :: Int
maxNamesToList = 10

data CollageJobs =
     CollageJobs
     { cjChatId :: Int64
     , cjMessageId :: Int64
     , cjFinalDraft :: Maybe Text
     , cjUrls :: [Text]
     , cjFabricNames :: [Text]
     } deriving Show

$(deriveJSON defaultOptions { fieldLabelModifier = recordLabelModifier "cj" } ''CollageJobs)

-- | This function runs in its own thread for the entire application lifetime.
runCollageJobListener :: PG.ConnectInfo -> (forall a. AppM a -> IO (Either ServerError a)) -> AppM ()
runCollageJobListener connInfo runAppM = do
  $(logTM) InfoS "CollageJobListener starts listening..."
  -- Get the underlying libpq connection
  liftIO $ PG.withConnect connInfo $ \conn -> do
    -- 1. Subscribe to the channel. This must be done on the connection.
    void $ PG.execute_ conn "LISTEN collage_jobs"
    -- 2. Loop forever, waiting for notifications.
    forever $ do
      -- PG.getNotification blocks until a message arrives.
      notification <- PG.getNotification conn
      let payload = PG.notificationData notification
      putStrLn $ "Received notification: " ++ show payload
      let eRes = eitherDecode @CollageJobs $ BL.fromStrict payload
      -- 3. Parse the payload (the ID of the digest)
      for_ eRes $ \collageJobs -> do
        -- 4. SPAWN THE WORKER THREAD
        -- IMPORTANT: We spawn a new async thread to do the actual work.
        -- This keeps the listener free to immediately wait for the next notification.
        -- You would run this in your AppM to get logging etc.
        -- For simplicity here, just showing the concept:
        void $ runAppM $ $(logTM) InfoS $ ls $ $currentModule <> ":CollageJobs " <> show collageJobs
        void $ Async.async $ generateAndAttachCollageAndOPublish_worker runAppM collageJobs
      when (isLeft eRes) $ putStrLn $ "Failed to parse payload, error: " <> show eRes

generateAndAttachCollageAndOPublish_worker :: (forall a. AppM a -> IO (Either ServerError a)) -> CollageJobs -> IO ()
generateAndAttachCollageAndOPublish_worker runAppM CollageJobs {..}
  | isNothing cjFinalDraft = void $ runAppM $ $(logTM) ErrorS $ "Failed to generate collage: empty body"
  | otherwise = do
      putStrLn $ "Processing collage job for chat " <> show cjChatId
      jobId <- randomIO @Word32
      eitherFilePath <- 
        fmap (join . first (T.pack . show)) $ runAppM $ do
          cfg <- ask
          let isOn = _isCollageServiceOn cfg
          let stubPath = _collageStubPath cfg
          let mgr = _configHttpManager cfg
          if isOn then generateCollageViaService cjUrls jobId
          else liftIO $ fmap Right $ getStubFilePath mgr stubPath
      case eitherFilePath of
        Left err -> void $ runAppM $ $(logTM) ErrorS $ ls $ "Failed to generate collage: " <> err
          -- Optionally, you could *edit the caption* to add an error note,
          -- but for simplicity, we'll just log it.
          
        Right collagePath -> do
          putStrLn "Collage generated. Swapping media in message..."

          now <- getCurrentTime
          let today = utctDay now
          let dateStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" today
          Right galleryLink <- runAppM $ fmap _galleryLink ask 
          let deepLinkUrl = galleryLink <> dateStr
          let keyboard = 
                object
                [ "inline_keyboard" .=
                  [[ object 
                      [ "text" .= ("✨ Посмотреть галерею ✨" :: T.Text)
                      , "url"  .= deepLinkUrl
                      ]
                  ]]
                ]

          -- --- THE CORE LOGIC (CORRECTED) ---
          -- We call the helper with 'Nothing' for the caption.
          -- Telegram will automatically keep the existing caption.
          let Just body = cjFinalDraft
          --- NEW LOGIC: TRUNCATE THE LIST ---
          let truncatedNames = truncateFabricNames cjFabricNames
          let finalText = escapeMarkdownV2 $ cleanDigestText body truncatedNames
          eResult <- runAppM $ sendPhotoToTelegram $currentModule finalText MAIN (Just keyboard) collagePath
          when(isLeft eResult) $ void $ runAppM $ $(logTM) ErrorS $ ls $ "Failed to update Telegram message: " <> show eResult
          removeFile collagePath
          void $ runAppM $ deleteMessage (fromIntegral cjMessageId) WAREHOUSE
          -- publish status
          void $ runAppM $ fmap _appDBPool ask >>= (liftIO . setDailyDigestStatus (DailyDigest cjChatId cjMessageId) Published)

-- | Removes known digest tags and surrounding whitespace from the input text.
--   It handles multiple possible tags like #digest
cleanDigestText :: Text -> Text -> Text 
cleanDigestText rawText bodyContent =
  let
     -- 1. Split the text into lines
     lines = T.lines rawText
  in
     case lines of
        -- If there are no lines, return empty text
        [] -> mempty
        -- Take the first line (header) and the rest of the lines (body)
        (_ : body) ->
          -- 2. Join the cleaned header and the original body back together
          -- We filter to remove the first line if it becomes empty after cleaning
          T.replace "#body" bodyContent $ T.unlines $ filter (not . T.null) body

truncateFabricNames :: [Text] -> Text
truncateFabricNames allFabricNames
  | length allFabricNames <= maxNamesToList =
    let numberedItems = zipWith (\n name -> T.pack (show n) <> ". " <> name) [1..] allFabricNames
    in T.unlines numberedItems
  | otherwise =  
    let truncatedList = take maxNamesToList allFabricNames
        -- Calculate how many are left over.
        remainingCount = length allFabricNames - maxNamesToList
        -- Create the footer text.
        footer = "...и еще " <> T.pack (show remainingCount) <> " позиций."
        numberedItems = zipWith (\n name -> T.pack (show n) <> ". " <> name) [1..] truncatedList
    in T.unlines numberedItems <> "\n" <> footer


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