{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handlers.GetDeliveryPoints(handler) where

import Data.Text (Text)

import Types (AppM)
import API.Types (ApiResponse, Providers, DeliveryPoint (..), mkError)


handler :: Providers -> Maybe Text -> AppM (ApiResponse [DeliveryPoint])
handler _ (Just _) = do 
 -- In a real implementation, you would:
  -- 1. Log the incoming request: logInfo $ "Fetching points for " <> show provider <> " in " <> city
  -- 2. Call the provider-specific module based on the 'provider' value.
  --    e.g., case provider of
  --             SDEK -> Sdek.getPoints city
  --             Boxberry -> Boxberry.getPoints city
  -- 3. Handle potential errors from those modules.

  -- For now, we return a hardcoded list of delivery points as a stub.
  let samplePoints = 
       [DeliveryPoint "sdek_EKB20" "На Малышева" "Пн-Пт 10:00-20:00, Сб 10:00-16:00" "620075, Россия, Свердловская область, Екатеринбург, ул. Малышева, 122",
        DeliveryPoint "sdek_EKB35" "Пункт на Ленина" "Пн-Вс 10:00-22:00" "620014, Россия, Свердловская область, Екатеринбург, пр. Ленина, 25"
       ]

  -- The response structure includes the "ok": true wrapper
  pure $ Right samplePoints
handler _ _ = return $ Left $ mkError "city not provided"