{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Domain.Inventory (adjustInventoryForOrder, InventoryResult(..), Template (..)) where


import Data.Text (Text, pack)
import Control.Monad.IO.Class (liftIO)
import Hasql.Transaction.Sessions (Mode (..))
import Data.Bifunctor (first)
import qualified Hasql.Transaction as Hasql
import Data.Traversable (for)
import Data.Aeson (Result (..))
import Control.Monad (join, void)
import Data.Either (lefts)
import Data.Maybe (isJust)


import App (AppM, _appDBPool, _thresholdMetres, render, _cutTolerance)
import Control.Monad.Reader.Class (ask)
import Infrastructure.Database 
       ( runTransaction
       , updateOrderStatusStatement
       , updatePaymentStatusStatement
       , adjustFabric
       , getOrderItemsForAdjustStatement
       , AdjustFabric (..))
import API.Types (OrderStatus (Paid))
import TH.Location (currentModule)
import qualified Data.HashMap.Strict as HM
import Infrastructure.Services.Tinkoff.Types.GetState (Status (CONFIRMED, PENDING))
import  Domain.Warehouse.Types (FabricType (..))


data Template = RollBranch (Maybe Int) (AppM Text) | PrecutBranch Int


data InventoryResult 
  = StockOK Int
  | FabricSoldOutOrPrecut Int [Template] -- We pass info back to create a nice message


adjustInventoryForOrder :: Text -> AppM (Either Text InventoryResult)
adjustInventoryForOrder orderId = do
  cfg <- ask
  let pool = _appDBPool cfg
  let thresholdMetres = _thresholdMetres cfg
  let cutTolerance = fromIntegral (_cutTolerance cfg) / 100.0
  eResult <- liftIO $ 
    fmap (first (pack . show)) $ 
      runTransaction pool Write $ 
        statements orderId thresholdMetres cutTolerance
  fmap join $ for eResult $ \aesonRes -> do
    res <- for aesonRes $ \(mId, adjFabrics) -> do
      let soldOutOrPrecut :: FabricType -> AdjustFabric -> Either Template ()
          soldOutOrPrecut fabricType AdjustFabric {..}
            | fabricType == Roll =
              if afIsSold == False &&
                afIsPreCutReq == False 
              then
                Right ()
              else
                  let mkFabricSoldOutOrPrecut wMId tpl tplData = 
                        Left $ RollBranch wMId (render ($currentModule <> tpl) tplData)
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
              | otherwise = Left $ PrecutBranch afWarehouseMessageId
      let res = lefts $ map (uncurry soldOutOrPrecut) adjFabrics
      return $ case res of 
        [] -> StockOK mId
        xs -> FabricSoldOutOrPrecut mId xs     
    return $ case res of
      Success inventory -> Right inventory
      Error err -> Left $ pack err

statements orderId thresholdMetres cutTolerance = do
  -- update order to paid
  mId <- (orderId, Paid) `Hasql.statement` updateOrderStatusStatement
  -- adjust fabric, orderId
  items <- orderId `Hasql.statement` getOrderItemsForAdjustStatement
  adjFabrics <- for items $ \(fId, prId, length) -> do
    let lengthWithTolerance = fmap (flip (+) cutTolerance) length
    let ft | isJust prId = PreCut
           | otherwise = Roll
    fmap (fmap (ft,)) $ (fId, prId, lengthWithTolerance, thresholdMetres) `Hasql.statement` adjustFabric
  -- update payment status to paid
  void $ (orderId, CONFIRMED, PENDING) `Hasql.statement` updatePaymentStatusStatement

  return $ fmap (mId,) $ sequence adjFabrics