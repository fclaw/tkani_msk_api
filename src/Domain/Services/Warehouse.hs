{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Domain.Services.Warehouse (ensureWarehousePlatformId) where

import Katip
import Data.Text (Text, toLower)
import Control.Monad (join, void)
import Data.Foldable (for_)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Data.Traversable (for)
import Data.Foldable (for_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)


import Text (tshow)
import App (AppM, _appDBPool, _yandexConfig, ChatKey (WAREHOUSE))
import Infrastructure.Services.Yandex.Warehouse
import Infrastructure.Services.Yandex (initWarehouse)
import Utils.Telegram.Markdown (escapeMarkdownV2)
import Infrastructure.Services.Yandex.Error (getError, getHttpException)
import Infrastructure.Database (getYandexWarehouseId, saveYandexWarehouseId)
import Infrastructure.Services.Yandex.Config (YandexConfig (..))
import qualified Infrastructure.Services.Yandex.Config as YA
import Infrastructure.Services.Telegram (sendOrEditTelegramMessage)
import Infrastructure.Services.Yandex.Types (PlatformStationId (..), WarehouseCreateResp (..), WarehouseCreateReq (..))


ensureWarehousePlatformId :: AppM (Either Text PlatformStationId)
ensureWarehousePlatformId = do
  cfg <- ask
  let pool = _appDBPool cfg
  let yaCfg = _yandexConfig cfg
  let wId = localWarehouseId yaCfg
  let postfix = warehousePostfix yaCfg
  let wIdWithPostfix = wId <> "_" <> toLower postfix
  eRes <- getYandexWarehouseId wIdWithPostfix pool
  case eRes of 
    Left err -> pure $ Left err
    Right maybeWarehouseId ->
      case maybeWarehouseId of
        Just warehouseId -> 
          pure $ Right $ PlatformStationId warehouseId
        Nothing          -> do
          cfg <- ask
          let yaCfg = _yandexConfig cfg
          let initReq = mkInitWarehouseReq yaCfg
          $(logTM) InfoS $ "ensureWarehousePlatformId: initReq --> " <> 
                           ls (encodePretty initReq)
          eYaResp <- initWarehouse initReq
          case eYaResp of
            Left err -> do
              let maybeHttpExcep = getHttpException err
              for_ maybeHttpExcep $ \excep -> do
                let errMsg = escapeMarkdownV2 $ "‼️ " <> getError excep
                void $ sendOrEditTelegramMessage mempty errMsg WAREHOUSE Nothing Nothing Nothing
              pure $ Left $ tshow err
            Right WarehouseCreateResp {stationId} -> do
              cfg <- ask
              let pool = _appDBPool cfg
              eDbRes <- saveYandexWarehouseId wIdWithPostfix stationId pool
              case eDbRes of 
                Left err -> pure $ Left err
                Right _  -> pure $ Right $ PlatformStationId stationId
 
mkInitWarehouseReq :: YandexConfig -> WarehouseCreateReq
mkInitWarehouseReq YandexConfig {..} =
 WarehouseCreateReq
 { clientWarehouseId = localWarehouseId <> "_" <> toLower warehousePostfix
  , contact          =
      WarehouseContact 
      { phone      = YA.phone contact
      , email      = Just $ YA.email contact
      , firstName  = YA.name contact
      , lastName   = YA.surname contact
      , patronymic = Nothing
      }
  , location         = 
       WarehouseLocation 
       { address = 
          defWarehouseAddress 
          { city     = YA.city address
          , house    = YA.house address
          , building = Just $ YA.building address
          , floor    = Just $ YA.floor address
          , street   = Just $ YA.street address
          }
       , coordinates = office
       }
  , name             = warehouseName <> " (" <> warehousePostfix <> ")"
  , merchantId       = Nothing
 }