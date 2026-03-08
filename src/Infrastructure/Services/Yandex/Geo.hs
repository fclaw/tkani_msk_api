{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Services.Yandex.Geo (calcBoundingBox, haversineDist, GeoPoint(..)) where


import Data.Aeson
import GHC.Generics (Generic)

import Text (camelToSnake)


jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelToSnake }


data GeoPoint = GeoPoint
     { latitude  :: Double -- ^ Latitude coordinate of the point
     , longitude :: Double -- ^ Longitude coordinate of the point
     } deriving (Show, Eq, Generic)

instance FromJSON GeoPoint where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON GeoPoint where
  toJSON = genericToJSON jsonOptions

-- | Calculates the 'from' (Top Left) and 'to' (Bottom Right) points
-- | for a box with radius 'r' in kilometers.
calcBoundingBox :: GeoPoint -> Double -> (GeoPoint, GeoPoint)
calcBoundingBox (GeoPoint cLat cLon) rKm =
    let 
        -- Constant: km per 1 degree of latitude
        kmPerDegLat = 111.32
        
        -- Latitude offset is constant
        deltaLat = rKm / kmPerDegLat
        
        -- Longitude offset shrinks as we move toward poles
        -- We convert latitude to Radians for the cos function
        latRadians = cLat * pi / 180
        deltaLon = rKm / (kmPerDegLat * cos latRadians)
        
        fromPoint = GeoPoint (cLat + deltaLat) (cLon - deltaLon)
        toPoint   = GeoPoint (cLat - deltaLat) (cLon + deltaLon)
    in 
        (fromPoint, toPoint)

-- Example usage:
-- let center = GeoPoint 55.7558 37.6173 -- Moscow
-- let (from, to) = calcBoundingBox center 2.0

-- | Calculates the Great-Circle distance (Haversine) between two points in KM
haversineDist :: GeoPoint -> GeoPoint -> Double
haversineDist p1 p2 =
    let r = 6371 -- Earth's radius in kilometers
        dLat = rad (latitude p2 - latitude p1)
        dLon = rad (longitude p2 - longitude p1)
        lat1 = rad (latitude p1)
        lat2 = rad (latitude p2)
        
        a = sin (dLat / 2) ** 2 + 
            cos lat1 * cos lat2 * sin (dLon / 2) ** 2
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    in r * c
  where
    rad d = d * pi / 180