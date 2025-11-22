{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Overpass.Geo (findNearestMetros) where

import Data.Text (Text)
import Data.List (sortBy)
import Data.Function (on)
import Infrastructure.Services.Overpass.Types (MetroStation(..))

-- | Earth radius in Meters
earthRadius :: Double
earthRadius = 6371000

-- | Convert Degrees to Radians
degToRad :: Double -> Double
degToRad d = d * pi / 180

-- | Haversine Formula: Calculate distance in meters between two (Lat, Lon) points
distMeters :: Double -> Double -> Double -> Double -> Double
distMeters lat1 lon1 lat2 lon2 = 
    let dLat = degToRad (lat2 - lat1)
        dLon = degToRad (lon2 - lon1)
        lat1r = degToRad lat1
        lat2r = degToRad lat2
        
        a = sin (dLat / 2) * sin (dLat / 2) +
            cos lat1r * cos lat2r *
            sin (dLon / 2) * sin (dLon / 2)
        
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    in earthRadius * c

-- | The Main Helper: Finds up to 3 stations within 1.5km
findNearestMetros :: Double -> Double -> [MetroStation] -> [Text]
findNearestMetros targetLat targetLon allMetros =
    let 
        -- 1. Calculate distance for ALL stations
        --    Result type: [(Distance, StationName)]
        withDist = map (\m -> (distMeters targetLat targetLon (msLat m) (msLon m), msName m)) allMetros
        
        -- 2. Filter: Only keep stations closer than 1500 meters
        nearby   = filter (\(d, _) -> d <= 1500) withDist
        
        -- 3. Sort: Closest first
        sorted   = sortBy (compare `on` fst) nearby
        
        -- 4. Take the top 3 and extract just the names
        top3     = take 3 sorted
    in 
    map snd top3