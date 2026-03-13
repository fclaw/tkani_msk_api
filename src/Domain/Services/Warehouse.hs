{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module Domain.Services.Warehouse (ensureWarehousePlatformId) where

import Data.Text (Text, toLower)
import Control.Monad (join)
import Control.Monad.Reader.Class (ask)
import Control.Monad.State.Class (get)
import Data.Traversable (for)
import Data.Foldable (for_)
import Control.Monad.IO.Class (liftIO)


import Text (tshow)
import App (AppM, _yandexWarehouseId, readTVarIO, modifyTVarIO, _appDBPool, _yandexConfig)
import Infrastructure.Services.Yandex.Warehouse
import Infrastructure.Services.Yandex (initWarehouse)
import Infrastructure.Database (getYandexWarehouseId, saveYandexWarehouseId)
import Infrastructure.Services.Yandex.Config (YandexConfig (..))
import qualified Infrastructure.Services.Yandex.Config as YA
import Infrastructure.Services.Yandex.Types (PlatformStationId (..), WarehouseCreateResp (..), WarehouseCreateReq (..))


ensureWarehousePlatformId :: AppM (Either Text PlatformStationId)
ensureWarehousePlatformId = do
  stateVar <- get
  maybeWarehouseId <- fmap _yandexWarehouseId $ readTVarIO stateVar
  case maybeWarehouseId of
    Just warehouseId -> pure $ Right warehouseId
    Nothing -> do
      cfg <- ask
      let pool = _appDBPool cfg
      let yaCfg = _yandexConfig cfg
      let wId = localWarehouseId yaCfg
      eRes <- getYandexWarehouseId wId pool
      fmap join $
        for eRes $ \maybeWarehouseId ->
          case maybeWarehouseId of
            Just warehouseId -> 
              pure $ Right $ PlatformStationId warehouseId
            Nothing           -> do
              cfg <- ask
              let yaCfg = _yandexConfig cfg
              let initReq = mkInitWarehouseReq yaCfg
              eYaResp <- initWarehouse initReq
              case eYaResp of
                Left err -> pure $ Left $ tshow err
                Right WarehouseCreateResp {stationId} -> do
                  cfg <- ask
                  let pool = _appDBPool cfg
                  eDbRes <- saveYandexWarehouseId wId stationId pool
                  case eDbRes of 
                    Left err -> pure $ Left err
                    Right _ -> do
                      stateTVar <- get
                      modifyTVarIO stateTVar $ \s -> 
                        s { _yandexWarehouseId = 
                            Just (PlatformStationId stationId) }
                      pure $ Right $ PlatformStationId stationId

mkInitWarehouseReq :: YandexConfig -> WarehouseCreateReq
mkInitWarehouseReq YandexConfig {..} =
 WarehouseCreateReq
 { clientWarehouseId = "tkani_msk_" <> toLower warehousePostfix 
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
          , street   = YA.street address 
          }
       , coordinates = office 
       }
  , name             = "Tkani MSK " <> "(" <> warehousePostfix <> ")" 
  , merchantId       = Nothing
 }