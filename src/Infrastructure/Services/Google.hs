{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Infrastructure.Services.Google (getGeocode) where


import Data.Text (Text, unpack)
import Control.Monad.Reader.Class (ask)


import App
import Infrastructure.Utils.Http (HttpError, getReq)
import Infrastructure.Services.Google.Geocode (GeocodingResponse)


getGeocode :: Text -> AppM (Either HttpError GeocodingResponse)
getGeocode address = do
  -- 1. Get configuration from the AppM ReaderT environment
  cfg <- ask
  let baseUrlText = _geocodeUrl cfg      -- e.g., "maps.googleapis.com/maps/api/geocode/json"
  let apiKey      = _geocodeApiKey cfg
  let httpManager = _configHttpManager cfg
  -- 2. Construct the URL and query parameters for the request
  let geocodeUrl = show HTTPS <> unpack baseUrlText
  let params = [("address", address), ("key", apiKey)]
  -- 3. Prepare the request object using your helper
  getReq @GeocodingResponse httpManager geocodeUrl params [] Nothing