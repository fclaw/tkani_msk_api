{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Handlers.SdekDeliveryPointUUID (handler) where


import Katip
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)



import App (AppM)
import API.Types (ApiResponse, ApiError (..), wrongParamsErrorCode)
import Infrastructure.Services.Google (getGeocode)

handler :: Maybe Text -> AppM (ApiResponse Text)
handler (Just address) = do 
  res <- getGeocode address
  liftIO $ print res
  return $ Right "sdc"
handler Nothing = fmap (const (Left $ ApiError wrongParamsErrorCode mempty)) $ $(logTM) ErrorS $ "parameter 'address' is missing."