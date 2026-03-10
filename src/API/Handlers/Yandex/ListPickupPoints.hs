{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.Yandex.ListPickupPoints (handler) where

import Katip 
import Data.Functor ((<&>))
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (liftIO)


import App (AppM)
import API.WithField (WithField (..))
import Infrastructure.Services.Yandex.Geo (latitude, longitude)
import Infrastructure.Services.Yandex.Types (fullAddress, ppId, ppName, ppAddress, ppPosition)
import Infrastructure.Services.Yandex.CachedPickupPoints (storeDeliveryPoints)
import API.Types (ApiResponse, mkError, YandexPickupPointsResp (..), DeliveryPoint (..), PointLocation (..), DisplayInfo (..))


-- | Targeted PVZ formatting for rural and urban areas
formatPvzUniversal :: T.Text -> T.Text
formatPvzUniversal raw =
  let -- 1. Basic cleaning and standard abbreviations
      clean = T.replace "рабочий посёлок" "рп."
            . T.replace "деревня" "д."
            . T.replace "село" "с."
            . T.replace "посёлок" "п."
            . T.replace "улица" "ул."
            . T.replace "стрт" "стр."
            $ raw
            
      ws = T.words clean
  in case ws of
    -- Logic A: Multi-word prefixes like "рп. Винзили"
    (prefix:name:rest) | prefix `elem` ["рп.", "п.", "д.", "с.", "г."] ->
        let identity = prefix <> " " <> name
            house    = last rest
            street   = T.unwords (init rest)
        in identity <> ", " <> street <> ", " <> house

    -- Logic B: Standard city like "Новосибирск Спортивная ул. 19"
    (city:rest) ->
        if length rest >= 2 then 
          let house  = last rest
              street = T.unwords (init rest)
          in street <> ", " <> house
        else clean
    _ -> clean


handler :: Maybe Int -> AppM (ApiResponse YandexPickupPointsResp)
handler Nothing = return $ Left $ mkError "geoId not provided"
handler (Just geoId) = do
  eRes <- storeDeliveryPoints geoId
  liftIO $ print eRes
  case eRes of
    Left err -> do
      $(logTM) ErrorS $ "Failed to fetch pickup points from Yandex" <> ls (show err)
      return $ Left $ mkError "Failed to fetch pickup points from Yandex"
    Right pickupPoints -> 
      let points = 
            catMaybes $ 
              pickupPoints <&> \(WithField metros point) ->
                ppId point <&> \code ->
                  let dp =
                        DeliveryPoint
                        { dpCode    = code
                        , dpName    = ppName point
                        , dpWorkTime = mempty -- Placeholder, replace with actual work time if available in the response
                        , dpHasDressingRoom = False -- Placeholder, replace with actual info if available in the response
                        , dpLocation = 
                           PointLocation
                           { locAddressFull = 
                               formatPvzUniversal $ 
                                 fullAddress $ 
                                   ppAddress point
                           , locLongitude = longitude (ppPosition point) -- Placeholder, replace with actual longitude if available in the response
                           , locLatitude = latitude (ppPosition point) -- Placeholder, replace with actual latitude if available in the response
                           }
                        , dpDisplay = DisplayInfo mempty mempty -- Placeholder, replace with actual display info if available in the response
                        }
                  in WithField metros dp
      in return $ Right $ YandexPickupPointsResp (length points) points