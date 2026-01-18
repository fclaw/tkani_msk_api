{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module API.Handlers.SdekDeliveryPointUUID (handler) where


import Katip
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)

import App (AppM)
import Infrastructure.Services.Google.Geocode
import Infrastructure.Services.Google (getGeocode)
import Infrastructure.Services.Sdek.Geocode (getNearestDeliveryPoint)
import Infrastructure.Services.Sdek.Types.Geocode (SdekPoint (..))
import Infrastructure.Services.Sdek.CachedDeliveryPoints (storeAllDeliveryPoints)
import API.Types (ApiResponse, ApiError (..), wrongParamsErrorCode, mkError)

handler :: Maybe Text -> AppM (ApiResponse Text)
handler (Just address) = do 
  res <- getGeocode address
  case res of
    Left err ->
      fmap (const (Left (mkError "server error"))) $ 
        $(logTM) ErrorS $ ls $ "Google geocode results in failure " <> show err
    Right GeocodingResponse {..} ->
      case status of
        OK ->
          case result of
            Nothing -> do
              $(logTM) WarningS $ "Google geocode results in OK but no results found" 
              return (Left (mkError "address not found"))
            Just res -> do 
              ePoints <- storeAllDeliveryPoints
              case ePoints of
                Left err -> do 
                  $(logTM) ErrorS $ ls $ "SDEK fetching all points results in error " <> show err
                  return $ Left (mkError "server error")
                Right points ->
                  let coords = extractGeoCoordinates res
                      point = uncurry getNearestDeliveryPoint coords $ points
                  in case point of
                    Nothing -> return (Left (mkError "address not found"))
                    Just SdekPoint {..} -> return (Right code)
        ZERO_RESULTS -> return (Left (mkError "address not found"))
        _ -> return (Left (mkError "server error"))
handler Nothing = fmap (const (Left $ ApiError wrongParamsErrorCode mempty)) $ $(logTM) ErrorS $ "parameter 'address' is missing."