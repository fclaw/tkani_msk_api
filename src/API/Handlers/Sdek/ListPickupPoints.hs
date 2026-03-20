{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}

module API.Handlers.Sdek.ListPickupPoints(handler) where

import Data.Text (Text)

import App (AppM)
import API.Types (ApiResponse, DeliveryPoint (..), mkError)
import Infrastructure.Services.Sdek (getDeliveryPoints)
import API.WithField (WithField (..))


handler :: Maybe Text -> AppM (ApiResponse [WithField "isPrepaid" Bool (WithField "dpMetros" [Text] DeliveryPoint)])
handler (Just city) = fmap (fmap (map (WithField False))) $ getDeliveryPoints city
handler _ = return $ Left $ mkError "city not provided"