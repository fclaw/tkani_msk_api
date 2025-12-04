{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Domain.Inventory (adjustInventoryForOrder, InventoryResult(..)) where


import Data.Text (Text, pack)
import Control.Monad.IO.Class (liftIO)
import Hasql.Transaction.Sessions (Mode (..))
import Data.Bifunctor (first)
import qualified Hasql.Transaction as Hasql
import Data.Traversable (for)
import Data.Aeson (Result (..))
import Control.Monad (join, void)


import App (AppM, _appDBPool, _thresholdMetres, render)
import Control.Monad.Reader.Class (ask)
import Infrastructure.Database 
       ( runTransaction
       , updateOrderStatusStatement
       , updatePaymentStatusStatement
       , adjustFabric
       , AdjustFabric (..))
import API.Types (OrderStatus (Paid))
import TH.Location (currentModule)
import qualified Data.HashMap.Strict as HM
import Infrastructure.Services.Tinkoff.Types.GetState (Status (CONFIRMED, PENDING))


data InventoryResult 
  = StockOK Int
  | FabricSoldOutOrPrecut Int (Maybe Int) (AppM Text) -- We pass info back to create a nice message


adjustInventoryForOrder :: Text -> AppM (Either Text InventoryResult)
adjustInventoryForOrder orderId = do
  cfg <- ask
  let pool = _appDBPool cfg
  let thresholdMetres =  _thresholdMetres cfg
  eResult <- liftIO $ 
    fmap (first (pack . show)) $ 
      runTransaction pool Write $ 
        statements orderId thresholdMetres
  fmap join $ for eResult $ \aesonRes -> do
    res <- for aesonRes $ \(mId, AdjustFabric {..}) ->
      return $ 
        if afIsSold == False &&
          afIsPreCutReq == False 
        then
          StockOK mId
        else
            let mkFabricSoldOutOrPrecut wMId tpl tplData = 
                  FabricSoldOutOrPrecut mId wMId (render ($currentModule <> tpl) tplData)
            in 
              if afIsSold == True && 
                 afIsPreCutReq == False
              then let templateData = 
                         HM.fromList 
                         [("fabricName", afName), 
                          ("article", afArticle)
                         ]
                   in mkFabricSoldOutOrPrecut (Just afWarehouseMessageId) ".Sold" templateData
              else let templateData = 
                         HM.fromList 
                         [ ("fabricName", afName), 
                           ("article", afArticle), 
                           ("remainingLength", pack (show afRemLength))
                         ]   
                   in mkFabricSoldOutOrPrecut Nothing ".Precut" templateData
    return $ case res of
      Success inventory -> Right inventory
      Error err -> Left $ pack err

statements orderId thresholdMetres = do
  -- update order to paid
  mId <- (orderId, Paid) `Hasql.statement` updateOrderStatusStatement
  -- adjust fabric
  adjFabric <- (orderId, thresholdMetres) `Hasql.statement` adjustFabric
  -- update payment status to paid
  void $ (orderId, CONFIRMED, PENDING) `Hasql.statement` updatePaymentStatusStatement

  return $ fmap (mId,) adjFabric