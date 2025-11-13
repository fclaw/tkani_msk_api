{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handlers.GetDeliveryPoints(handler) where

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)


import Types (AppM)
import API.Types (ApiResponse, Providers (..), DeliveryPoint (..), mkError)
import Provider.Sdek (getDeliveryPoints)


handler :: Providers -> Maybe Text -> AppM (ApiResponse [DeliveryPoint])
handler SDEK (Just city) = liftIO $ getDeliveryPoints city
handler _ (Just _) = return $ Left $ mkError "None provider has been hit"
handler _ _ = return $ Left $ mkError "city not provided"