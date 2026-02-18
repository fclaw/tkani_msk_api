{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}

module API.Handlers.GetSdekPointFullAddress(handler) where

import Katip
import Data.Text (Text)
import Data.List (find)

import App (AppM, SdekPointCode (..))
import API.Types (ApiResponse, ApiError (..), wrongParamsErrorCode)
import Infrastructure.Services.Sdek.CachedDeliveryPointsCodes (fetchCodes)


-- The handler function itself is the same as before.
-- It runs in our AppM monad.
handler :: Maybe Text -> AppM (ApiResponse (Maybe Text))
handler Nothing = do 
  $(logTM) ErrorS $ "invalid params in Handlers.GetSdekPointFullAddress: Nothing"
  return $ Left $ ApiError wrongParamsErrorCode mempty
handler (Just code) = do
  codes <- fetchCodes
  let address = flip find codes $ \SdekPointCode {..} -> spcCode == code
  return $ Right $ fmap spcAddress address