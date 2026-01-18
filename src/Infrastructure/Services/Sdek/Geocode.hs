module Infrastructure.Services.Sdek.Geocode (getNearestDeliveryPoint) where

import Data.List (minimumBy)
import Data.Function (on)

import Infrastructure.Services.Sdek.Types.Geocode (SdekPoint (..))



-- | Finds the closest SdekPoint to a given latitude and longitude from a list of points.
--
--   This is a pure function:
--   - It is deterministic: for the same inputs, it always returns the same output.
--   - It has no side effects.
--   - It handles all cases, returning 'Nothing' for an empty list of points.
--
--   Args:
--     userLat: The latitude of the user's location.
--     userLon: The longitude of the user's location.
--     points: A list of 'SdekPoint's to search through.
--
--   Returns:
--     'Just' the nearest 'SdekPoint' if the input list is not empty.
--     'Nothing' if the input list is empty.
getNearestDeliveryPoint :: Double -> Double -> [SdekPoint] -> Maybe SdekPoint
getNearestDeliveryPoint _ _ [] = Nothing -- Base case: If the list is empty, there is no nearest point.
getNearestDeliveryPoint userLat userLon points =
  let
    -- A helper function that calculates the distance from the user's
    -- coordinates to a single SdekPoint. It "captures" userLat and userLon
    -- from the parent scope.
    distanceToPoint :: SdekPoint -> Double
    distanceToPoint p = haversineDistance (userLat, userLon) (latitude p, longitude p)

  in
    -- 'minimumBy' finds the minimum element in a list based on a custom comparison.
    -- The comparison is built using 'on', which adapts a function (in this case, 'compare')
    -- to work on the results of another function ('distanceToPoint').
    -- This elegantly reads as: "find the minimum point by comparing their distances to the user".
    Just $ minimumBy (compare `on` distanceToPoint) points


-- | Calculates the great-circle distance between two points on the earth
--   (specified in decimal degrees) using the Haversine formula.
--
--   Args:
--     (lat1, lon1): A tuple for the first coordinate.
--     (lat2, lon2): A tuple for the second coordinate.
--
--   Returns:
--     The distance between the two points in kilometers.
haversineDistance :: (Double, Double) -> (Double, Double) -> Double
haversineDistance (lat1, lon1) (lat2, lon2) =
  let
    earthRadiusKm = 6371.0

    dLat = degToRad (lat2 - lat1)
    dLon = degToRad (lon2 - lon1)

    lat1_rad = degToRad lat1
    lat2_rad = degToRad lat2

    a = sin (dLat / 2) * sin (dLat / 2) +
        cos lat1_rad * cos lat2_rad *
        sin (dLon / 2) * sin (dLon / 2)

    c = 2 * atan2 (sqrt a) (sqrt (1 - a))

  in earthRadiusKm * c

-- | Helper function to convert degrees to radians.
degToRad :: Double -> Double
degToRad deg = deg * (pi / 180)