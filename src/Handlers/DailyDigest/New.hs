{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.DailyDigest.New (handler) where

import Katip
import qualified Data.Text as T
import Data.Time.Clock (utctDay)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Foldable (for_)
import Data.Aeson
import qualified Data.Map.Strict as M
import Control.Monad.Reader.Class (ask)
import Control.Monad (void, when, unless)
import qualified Data.HashMap.Strict as HM
import Data.Bifunctor (second)
import Data.Either (isLeft)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)

import App (AppM, currentTime, ChatKey(WAREHOUSE), _bots, render, _appDBPool, _dailyDigestImgStub, _galleryLink, _configHttpManager)
import API.Types (ApiResponse)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendPhotoToTelegram, MessageIdResponse (..))
import TH.Location (currentModule)
import Infrastructure.Database (saveDailyDigestDraft, checkDailyDigestDraft)
import Utils.CollageMaker (downloadImage)
import System.FilePath ((</>))
import Data.Word (Word32)


handler :: AppM ()
handler = do 
  $(logTM) InfoS $ ls @String $ "announcement is being drafted.."
  now <- currentTime
  let today = utctDay now
  let dateStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" today
  -- Construct the Deep Link URL
  pool <- fmap _appDBPool ask
  
  eRes <- liftIO $ checkDailyDigestDraft today pool
  for_ eRes $ \isAlready -> do
    when isAlready $ $(logTM) InfoS $ ls @String $ "announcement has already been registered"
    unless isAlready $ do
      cfg <- ask
      let bots = _bots cfg
      let galleryLink = _galleryLink cfg 
      let botsInfo = M.lookup WAREHOUSE bots
      for_ botsInfo $ \(bot, chatId) -> do
        let deepLinkUrl = galleryLink <> dateStr
        -- Construct the 'reply_markup' JSON for the button
        let keyboard = 
              object
              [ "inline_keyboard" .=
                 [[ object 
                      [ "text" .= ("✨ Посмотреть галерею ✨" :: T.Text)
                      , "url"  .= deepLinkUrl
                      ]
                ]]
              ]

        cfg <- ask
        let filePath = _dailyDigestImgStub cfg
        let mgr = _configHttpManager cfg
        fId <- liftIO $ randomIO @Word32
        let tmpDir = "/tmp/stub_" <> show fId
        liftIO $ createDirectoryIfMissing True tmpDir
        liftIO $ downloadImage mgr tmpDir (1, filePath)
        let img = tmpDir </> "img_" <> show 1 <> ".jpg"

        message <- fmap escapeMarkdownV2 $ render $currentModule $ HM.fromList [("date", dateStr)]
        eResp <- sendPhotoToTelegram $currentModule message WAREHOUSE (Just keyboard) img
        liftIO $ removeDirectoryRecursive tmpDir
        for_ eResp $ \(MessageIdResponse messageId) -> do
          eRes <- liftIO $ saveDailyDigestDraft today (fromIntegral chatId) (fromIntegral messageId) pool
          when(isLeft eRes) $ $(logTM) ErrorS $ ls $ "drafting announcement has failed. " <> show eRes
  when(isLeft eRes) $ $(logTM) ErrorS $ ls $ "drafting announcement has failed. " <> show eRes