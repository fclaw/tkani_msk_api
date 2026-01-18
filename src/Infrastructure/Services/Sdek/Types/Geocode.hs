{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Sdek.Types.Geocode where


import Data.Text (Text)
import Data.Aeson

-- | Represents a SDEK delivery point with essential information.
-- We can add more fields like 'address' or 'name' later without
-- affecting the logic of getNearestDeliveryPoint.
data SdekPoint = SdekPoint
    { code      :: Text
    , latitude  :: Double
    , longitude :: Double
    , address   :: Text -- Added for more useful output
    } deriving (Show, Eq)


-- | FromJSON instance for SdekPoint
--   We write this manually to handle the nested 'location' object in the JSON response.
instance FromJSON SdekPoint where
  parseJSON = withObject "SdekPoint" $ \v -> do
    -- First, parse the nested 'location' object from the top-level object 'v'.
    locationObj <- v .: "location"

    -- Now, construct our SdekPoint.
    -- The 'uuid' comes from the top-level object 'v'.
    -- The 'latitude', 'longitude', and 'address_full' come from the nested 'locationObj'.
    SdekPoint
      <$> v .: "code"
      <*> locationObj .: "latitude"
      <*> locationObj .: "longitude"
      <*> locationObj .: "address_full"