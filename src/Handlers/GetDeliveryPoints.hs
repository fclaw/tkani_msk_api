{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}

module Handlers.GetDeliveryPoints(handler) where

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import App (AppM)
import API.Types (ApiResponse, Providers (..), DeliveryPoint (..), mkError)
import Infrastructure.Services.Sdek (getDeliveryPoints)
import API.WithField (WithField)


handler :: Providers -> Maybe Text -> AppM (ApiResponse [WithField "dpMetros" [Text] DeliveryPoint])
handler SDEK (Just city) = getDeliveryPoints city
handler _ (Just _) = return $ Left $ mkError "None provider has been hit"
handler _ _ = return $ Left $ mkError "city not provided"