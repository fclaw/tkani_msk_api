{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.Yandex.ListPickupPoints (handler) where

import Katip 
import Data.Functor ((<&>))
import Data.Bifunctor (first)
import Data.Text (Text)
import Control.Monad (join)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (liftIO)


import App (AppM)
import API.WithField (WithField (..))
import Infrastructure.Services.Yandex.Geo (latitude, longitude)
import Infrastructure.Services.Yandex.Types (fullAddress, ppId, ppName, ppAddress, ppPosition, ppPaymentMethods)
import Infrastructure.Services.Yandex.CachedPickupPoints (storeDeliveryPoints)
import Infrastructure.Services.Yandex.Types.Enums (PaymentMethod (AlreadyPaid, CardOnReceipt, PostPay))
import API.Types (ApiResponse, mkError, YandexPickupPointsResp (..), DeliveryPoint (..), PointLocation (..), DisplayInfo (..))


-- | Formats Yandex/CDEK raw address strings for professional UI buttons.
-- | Handles rural settlements, urban-type settlements (пгт), and filler city tokens.
formatPvzUniversal :: T.Text -> T.Text
formatPvzUniversal raw =
  let -- 1. Basic cleaning and common Russian administrative abbreviations
      clean = T.replace "рабочий посёлок" "рп."
            . T.replace "посёлок городского типа" "пгт"
            . T.replace "деревня" "д."
            . T.replace "село" "с."
            . T.replace "посёлок" "п."
            . T.replace "улица" "ул."
            . T.replace "стрт" "стр."
            $ raw
            
      ws = T.words clean
      
      -- Set of tokens that denote 'city' which are redundant if found in the middle
      isFiller w = w `elem` ["г", "г.", "гор"]
  in case ws of
    -- CASE 1: City + Filler + Street (e.g., "Сургут г Энгельса ул. 11")
    -- We drop BOTH the city and the filler 'г'.
    (city:filler:streetStart:rest) | isFiller filler ->
        let house  = last rest
            street = T.unwords (streetStart : init rest)
        in street <> ", " <> house

    -- CASE 2: Multi-word Identity with prefixes (e.g., "пгт. Малаховка", "д. Патрушева")
    -- We keep the Identity but format the street and house clearly.
    (prefix:name:rest) | prefix `elem` ["рп.", "пгт", "п.", "д.", "с."] ->
        if null rest 
        then prefix <> " " <> name
        else let house  = last rest
                 street = T.unwords (init rest)
             in prefix <> " " <> name <> ", " <> street <> ", " <> house

    -- CASE 3: Standard City Format (e.g., "Новосибирск Спортивная 19")
    -- We hide the City and show only the internal location details.
    (city:rest) | length rest >= 2 ->
        let house  = last rest
            street = T.unwords (init rest)
        in street <> ", " <> house

    -- FALLBACK: For extremely short strings, return the cleaned version.
    _ -> clean


handler :: Maybe Int -> AppM (ApiResponse YandexPickupPointsResp)
handler Nothing = return $ Left $ mkError "geoId not provided"
handler (Just geoId) = do
  eRes <- storeDeliveryPoints geoId
  case eRes of
    Left err -> do
      $(logTM) ErrorS $ "Failed to fetch pickup points from Yandex" <> ls (show err)
      return $ Left $ mkError "Failed to fetch pickup points from Yandex"
    Right pickupPoints -> 
      let points = 
            catMaybes $ 
              pickupPoints <&> \(WithField metros point) ->
                join $ ppId point <&> \code ->
                  let onSitePayments = [ p | p <- ppPaymentMethods point, p == CardOnReceipt || p == PostPay ]
                      isPrepaid = (AlreadyPaid `elem` ppPaymentMethods point) && null onSitePayments
                      dp =
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
                  in if length onSitePayments > 0 || isPrepaid then
                       Just $ WithField isPrepaid $ WithField metros dp
                     else Nothing  
      in return $ Right $ YandexPickupPointsResp (length points) points