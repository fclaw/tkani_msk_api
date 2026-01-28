{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RecordWildCards   #-}

module Workers.SpecialPostManager (runSpecialPostManager, buttonTextForLifecycle) where

import Katip
import Data.Int (Int64)
import Data.Text (Text, isInfixOf)
import Data.Word (Word32)
import Data.Either (isLeft, fromLeft)
import Data.Aeson ((.=), object)
import Control.Monad (forever, when, void)
import Data.Foldable (for_)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import System.Random (randomIO)
import Data.Time.Clock (utctDay, diffUTCTime)
import System.Directory (removeFile)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (ask)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.Lifted (threadDelay)
import Network.HTTP.Client (HttpException (..))
import Control.Exception (SomeException (..), fromException)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Domain.Error (handleHttpError)
import TH.Location (currentModule)
import Text (tshow, encodeToText)
import Infrastructure.Services.Telegram
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Utils.CollageMaker (generateCollageViaService)
import Domain.Warehouse.Enums (FabricLifecycle (..))
import Infrastructure.Database (fetchSpecialPostDetails, insertNewSpecialPost, deleteSpecialPost, SpecialPostDetails (..), SpecialPostDetailsItems (..))
import App (AppM, _postsCfgs, _thresholdMetres, _messageCannotBeDeleted, _appDBPool, extractFromEither, render, currentTime, ChatKey (MAIN, ORDER), _conciergeBotUrl)



maxNamesToList :: Int
maxNamesToList = 10


runSpecialPostManager :: AppM ()
runSpecialPostManager = do
  $(logTM) InfoS "Special Post Manager started..."
  cfg <- ask
  let postsCfgs = _postsCfgs cfg -- list of type (FabricLifecycle, (lifetime, item threshold))
  let posts = fst $ unzip postsCfgs
  forever $ do
    for_ posts $ \lifeCycle -> do
      -- fall back on default values 7 days and 10 items
      let (lifeTime, itemThreshold) = 
            fromMaybe (7, 10) $
              lookup lifeCycle postsCfgs
      managePost lifeCycle lifeTime itemThreshold
    liftIO $ threadDelay (10 * 60 * 1000000) -- Wait 30 minutes
  

-- | Returns the appropriate button text for a given lifecycle state.
buttonTextForLifecycle :: FabricLifecycle -> Text
buttonTextForLifecycle lifecycle =
  case lifecycle of
    -- For new items, create excitement and highlight novelty.
    NewArrival -> "✨ Посмотреть новинки ✨"  -- View New Arrivals
    Advertised -> "🔔 Узнать о поступлении 🔔" -- Notify me about Arrival
    
    -- For regular items, the default text is perfect.
    Regular    -> "💎 Открыть каталог 💎"   -- Open Catalog
    
    -- For sale items, emphasize the discount.
    OnSale     -> "🔥 Перейти к скидкам 🔥"  -- Go to Discounts
    Clearance  -> "💥 Забрать остатки 💥"     -- Grab the Remnants / Final Stock
    
    -- Archived items shouldn't have a button, but we provide a fallback.
    Archived   -> "Архив"                      -- Archive (should not be used)

  
-- | Returns the appropriate header caption for a given lifecycle state.
captionForLifecycle :: FabricLifecycle -> Text
captionForLifecycle lifecycle =
  case lifecycle of
    NewArrival -> "💫 НОВОЕ ПОСТУПЛЕНИЕ 💫"      -- NEW ARRIVAL
    Advertised -> "✨ СКОРО В ПРОДАЖЕ ✨"         -- COMING SOON
    Regular    -> "💎 АССОРТИМЕНТ КАТАЛОГА 💎" -- CATALOG ASSORTMENT
    OnSale     -> "🔥 СЕЗОННАЯ РАСПРОДАЖА 🔥"  -- SEASONAL SALE
    Clearance  -> "💥 ЛИКВИДАЦИЯ ОСТАТКОВ 💥"    -- LIQUIDATION OF REMNANTS
    Archived   -> "АРХИВ"


truncateFabricNames :: [SpecialPostDetailsItems] -> Text
truncateFabricNames allFabricNames
  | length allFabricNames <= maxNamesToList =
    let numberedItems = zipWith makeName [1..] allFabricNames
    in T.unlines numberedItems
  | otherwise =  
    let truncatedList = take maxNamesToList allFabricNames
        -- Calculate how many are left over.
        remainingCount = length allFabricNames - maxNamesToList
        -- Create the footer text.
        footer = "...и еще " <> T.pack (show remainingCount) <> " позиций."
        numberedItems = zipWith makeName [1..] truncatedList
    in T.unlines numberedItems <> "\n" <> footer
  where makeName n SpecialPostDetailsItems {..} = T.pack (show n) <> ". " <> name <> " -" <> tshow discount <> "% "  <> "🔥"


managePost :: FabricLifecycle -> Int -> Int -> AppM ()
managePost lifeCycle lifeTime itemThreshold = do
  $(logTM) DebugS $ ls $
    "lifeCycle: " <> tshow lifeCycle <>
    " lifeTime: " <> tshow lifeTime <> 
    " itemThreshold: " <> tshow itemThreshold

  cfg <- ask
  let pool = _appDBPool cfg
  let threshold = _thresholdMetres cfg

  eDbRes <- fetchSpecialPostDetails lifeCycle threshold pool
  extractFromEither eDbRes $ \SpecialPostDetails {..} -> do
    case messageId of
      Nothing ->
       -- =======================================================
       -- === Option 2: No active post, not enough items.
       -- =======================================================
       if itemsCount < fromIntegral itemThreshold
       then $(logTM) InfoS $ ls $ 
              "No active post for " <> tshow lifeCycle <> 
              " and fewer than " <> tshow itemThreshold <> 
              " items. Skipping."
       -- =======================================================
       -- === Option 1: No active post, but enough items to justify one.
       -- =======================================================
       else do 
         $(logTM) InfoS $ ls $ 
           "No active post for " <> tshow lifeCycle <> 
           ", but " <> tshow itemsCount <> 
           " items found. Creating post."
         jobId <- liftIO $ randomIO @Word32
         eCollageRes <- generateCollageViaService randomThumbnailUrls jobId
         case eCollageRes of
           Left err -> $(logTM) ErrorS $ ls $ "Failed to generate collage: " <> tshow err
           Right path -> do 
            -- make a button with deep link
            let botUrl = _conciergeBotUrl cfg
            let deepLinkUrl = botUrl <> "?start=" <> encodeToText lifeCycle
            let buttonText = buttonTextForLifecycle lifeCycle
            let keyboard = 
                 object
                 [ "inline_keyboard" .=
                   [[ object 
                      [ "text" .= buttonText
                      , "url"  .= deepLinkUrl
                      ]
                   ]]
                 ]
            
            -- date
            now <- currentTime
            let today = utctDay now
            let dateStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" today

            let templateData = 
                  HM.fromList
                  [ ("date", dateStr)
                  , ("caption", captionForLifecycle lifeCycle)
                  , ("body", truncateFabricNames items)
                  ]
            msg <- fmap escapeMarkdownV2 $ render $currentModule templateData
            eTelRes <- sendPhotoToTelegram $currentModule msg MAIN (Just keyboard) path
            liftIO $ removeFile path
            when(isLeft eTelRes) $ 
              $(logTM) ErrorS $ ls $ 
                "failed to send post to telegram " <> 
                tshow (fromLeft undefined eTelRes)
            for_ eTelRes $ \MessageIdResponse {..} -> do
              insertNewSpecialPost message_id lifeCycle pool
              $(logTM) InfoS $ ls $ "post " <> tshow lifeCycle <> " has been successfully published"
      -- ===============================================
      -- Case: AN ACTIVE POST ALREADY EXISTS
      -- ===============================================
      Just messageId -> do
        if itemsCount == 0 then do
          $(logTM) InfoS $ ls $ 
            "Active post " <> tshow messageId <> 
            " for " <> tshow lifeCycle <> 
            " exists, but item count is zero. Deleting."
          deleteAndNotify lifeCycle messageId
        else do
          -- Items still exist. Now check if the post is expired.
          now <- currentTime
          let err = error "Database inconsistency: messageId exists but postedAt is null"
          let postAgeDays = diffUTCTime now (fromMaybe err postedAt) / (24 * 3600) -- Age in days
          
          if postAgeDays > 
             fromIntegral lifeTime then do
            $(logTM) InfoS $ ls $ 
              "Active post " <> tshow messageId <> 
              " for " <> tshow lifeCycle <> 
              " has expired. Deleting."
            deleteAndNotify lifeCycle messageId
          else $(logTM) InfoS $ ls $ 
                 "Active post " <> tshow messageId <> 
                 " for " <> tshow lifeCycle <> " is still valid."


-- | Helper function for the "safe delete" logic.
deleteAndNotify :: FabricLifecycle -> Int64 -> AppM ()
deleteAndNotify lifeCycle msgId = do
  -- 1. Attempt to delete the message from Telegram.
  eTelRes <- deleteMessage msgId MAIN
  for_ eTelRes $ const $ do
    -- 2a. Deletion succeeded. Remove the record from our database.
    $(logTM) InfoS $ ls $ "Successfully deleted post " <> tshow msgId <> ". Removing DB record."
    pool <- fmap _appDBPool ask
    deleteSpecialPost msgId pool
  when(isLeft eTelRes) $ do
    let Left ex = eTelRes
    case ex of 
      ApiRequestFailed someEx ->
        case fromException @HttpException someEx of 
          Nothing -> $(logTM) ErrorS $ ls $ "failed to send the message " <> show ex
          Just httpErr -> do
            msg <- fmap _messageCannotBeDeleted ask
            let errorText = handleHttpError httpErr
            if msg `isInfixOf` errorText
            then do
              -- Handle the fallback logic as before
              $(logTM) ErrorS $ ls $ "CRITICAL: Failed to delete post " <> tshow msgId <> ". Notifying admins."
              eTelRes <- forwardTelegramMessage mempty ORDER MAIN msgId
              for_ eTelRes $ \ForwardMessageResponse {..} -> do
                when(ok) $ do
                  let notice = 
                        escapeMarkdownV2 $ 
                          "ACTION REQUIRED: The bot failed to delete this expired post for " <> 
                          tshow lifeCycle <> 
                          ". Please delete it manually."
                  void $ sendOrEditTelegramMessage mempty notice ORDER Nothing (Just (message_id result)) Nothing
            else
                -- It's a different, more serious error.
                $(logTM) CriticalS $ ls $ "CRITICAL: Failed to send notification for " <> tshow msgId <> ". " <> errorText
      _ ->  $(logTM) ErrorS $ ls $ "failed to send the message " <> show ex
  