{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DeriveAnyClass             #-}

module Workers.InventoryStagnationJanitor (runInventoryStagnationJanitor) where


import Katip
import Control.Monad (void, when)
import qualified Data.Text as T
import Control.Monad.Reader.Class (ask)

import Text (tshow)
import TH.Location (currentModule)
import qualified Data.HashMap.Strict as HM
import App (AppM, ChatKey(MAIN), _appDBPool, _thresholdMetres, render)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Infrastructure.Database (fetchStallingFabrics, setDiscountOnStallingFabrics)


runInventoryStagnationJanitor :: AppM ()
runInventoryStagnationJanitor = do
  $(logTM) InfoS "Starting inventory stagnation janitor..."
  cfg <- ask
  let pool = _appDBPool cfg
  let threshold = _thresholdMetres cfg
  eRes <- fetchStallingFabrics threshold pool
  case eRes of
    Left err -> 
      $(logTM) ErrorS $ 
        "Failed to fetch stalling \
        \ fabrics: " <> ls err
    Right stallingFabrics ->
      when(length stallingFabrics > 0) $ do
        -- set 10% discount
        let ids = [id | (id, _, _, _) <- stallingFabrics]
        eDbRes <- setDiscountOnStallingFabrics ids pool
        case eDbRes of
          Left err -> 
            $(logTM) ErrorS $ 
              "Failed to set discount on stalling \
              \ fabrics: " <> ls err
          Right _ -> do
            let details = [(art, name, hash) | (_, art, name, hash) <- stallingFabrics ]
            let formatLine (art, name, hash) = 
                  let url = "https://t.me/tkaniMskConciergeBot?start=regular_" <> tshow hash
                  in "• [" <> escapeMarkdownV2 art <> "](" <> url <> ") — " <> escapeMarkdownV2 name
            let itemList = T.unlines $ map formatLine details
            let templateData = 
                    HM.fromList
                    [ ("discount", tshow 10)
                    , ("itemList", itemList)
                    ]
            msg <- render $currentModule templateData
            void $ sendOrEditTelegramMessage mempty msg MAIN Nothing Nothing Nothing

