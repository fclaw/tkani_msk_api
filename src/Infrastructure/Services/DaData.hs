{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Infrastructure.Services.DaData (verifyAddress) where


import Data.Text (Text, unpack)
import Control.Monad.Reader.Class (ask)


import App
import Infrastructure.Utils.Http (HttpError, postReq)
import Infrastructure.Services.DaData.StandardisedAddress (RawAddress, AddressVerificationResult)


verifyAddress :: RawAddress -> AppM (Either HttpError AddressVerificationResult)
verifyAddress request = do
  -- 1. Get configuration from the AppM ReaderT environment
  cfg <- ask
  -- ..
  undefined