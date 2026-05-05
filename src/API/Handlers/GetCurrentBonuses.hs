{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.GetCurrentBonuses (handler) where

import Katip
import Data.Int (Int64)
import Data.Bifunctor (bimap)
import Control.Monad.Reader.Class (ask)


import App (AppM, _appDBPool)
import Infrastructure.Database (getCurrentBonuses)
import API.Types (ApiResponse, CurrentBonusesResp (..), mkError)


handler :: Int64 -> AppM (ApiResponse CurrentBonusesResp)
handler userId = do
  pool <- fmap _appDBPool ask
  eDbRes <- getCurrentBonuses userId pool
  case eDbRes of
    Left err -> do
      $(logTM) ErrorS $ "GetCurrentBonuses db failed: " <> ls (show err)
      return $ Left $ mkError err
    Right (totalBonuses, cartPrice) ->
      return $ Right $ 
        -- 1. Determine the hard ceiling (50% of the cart price)
        let limitFromCart = cartPrice `div` 2
      
        -- 2. The amount they can actually spend is the MINIMUM of:
        --    a) All their bonuses
        --    b) The 50% ceiling
            spendable = min totalBonuses limitFromCart
      
        in  CurrentBonusesResp { 
              cbrTotalBalance = totalBonuses
            , cbrMaxSpendable = spendable 
            }