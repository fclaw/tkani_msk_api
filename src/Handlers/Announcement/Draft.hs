{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.Announcement.Draft (handler) where

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

import App (AppM, currentTime, ChatKey(WAREHOUSE), _bots, render, _appDBPool)
import API.Types (ApiResponse)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage, MessageIdResponse (..))
import TH.Location (currentModule)
import Infrastructure.Database (saveAnnouncementDraft, checkAnnouncementDraft)


handler :: AppM ()
handler = do 
  $(logTM) InfoS $ ls @String $ "announcement is being drafted.."
  now <- currentTime
  let today = utctDay now
  let dateStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" today
  -- Construct the Deep Link URL
  pool <- fmap _appDBPool ask
  
  eRes <- liftIO $ checkAnnouncementDraft today pool
  for_ eRes $ \isAlready -> do
    when isAlready $ $(logTM) InfoS $ ls @String $ "announcement has already been registered"
    unless isAlready $ do
      bots <- fmap _bots ask
      let botsInfo = M.lookup WAREHOUSE bots
      for_ botsInfo $ \(bot, chatId) -> do
        let deepLinkUrl = "https://t.me/tkaniMskConciergeBot" <> "?start=gallery_" <> dateStr
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
        message <- fmap escapeMarkdownV2 $ render ($currentModule <> ".Draft") $ HM.fromList [("date", dateStr)]
        eResp <- sendOrEditTelegramMessage $currentModule message WAREHOUSE Nothing Nothing (Just keyboard)
        for_ eResp $ \(MessageIdResponse messageId) -> do
          eRes <- liftIO $ saveAnnouncementDraft today (fromIntegral chatId) (fromIntegral messageId) pool
          when(isLeft eRes) $ $(logTM) ErrorS $ ls $ "drafting announcement has failed. " <> show eRes
  when(isLeft eRes) $ $(logTM) ErrorS $ ls $ "drafting announcement has failed. " <> show eRes