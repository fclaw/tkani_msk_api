{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Overpass.Types where

import Data.Text (Text)
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics


-- | Top level response
data OverpassResponse = OverpassResponse
  { opElements :: [OverpassNode]
  } deriving (Show, Generic)

instance FromJSON OverpassResponse where
  parseJSON = withObject "OverpassResponse" $ \v -> 
    OverpassResponse <$> v .: "elements"

-- | A single map node (Station)
data OverpassNode = OverpassNode
  { opTags :: Maybe (Map Text Text) -- Tags might be missing
  , opLat  :: Double
  , opLon  :: Double
  } deriving (Show, Generic)

instance FromJSON OverpassNode where
  parseJSON = withObject "OverpassNode" $ \v -> 
    OverpassNode <$> v .:? "tags" <*> v .: "lat" <*> v .: "lon"

-- | A single metro station
data MetroStation = MetroStation
  { msName :: Text
  , msLat  :: Double
  , msLon  :: Double
  } deriving (Show, Eq)

-- | Helper to extract list of names
extractOverpassMetros :: OverpassResponse -> [MetroStation]
extractOverpassMetros resp = 
   [ MetroStation name (opLat node) (opLon node)
    | node <- opElements resp
    , Just tags <- [opTags node]
    , Just name <- [Map.lookup "name" tags]
    -- Also filter out entrances if you only want center stations?
    -- For now, keep everything.
    ]


-- | MinLat, MinLon, MaxLat, MaxLon
type BoundingBox = (Double, Double, Double, Double)

data CityTarget = CityTarget
  { ctSdekCode :: Int
  , ctName     :: Text
  , ctBbox     :: BoundingBox
  }

-- | Defined areas for Russian cities with Metros
metroCities :: [CityTarget]
metroCities = 
    [ CityTarget 44 "Moscow"            (55.10, 36.80, 56.05, 38.20)
    , CityTarget 137 "Saint Petersburg" (59.70, 29.50, 60.30, 30.80)
    , CityTarget 157 "Nizhny Novgorod"  (56.10, 43.70, 56.40, 44.10)
    , CityTarget 172 "Novosibirsk"      (54.80, 82.70, 55.15, 83.20)
    , CityTarget 232 "Samara"           (53.10, 50.00, 53.35, 50.30)
    , CityTarget 261 "Yekaterinburg"    (56.70, 60.35, 57.00, 60.80)
    , CityTarget 270 "Kazan"            (55.70, 49.00, 55.90, 49.30)
    -- Volgograd (Metrotram) might need specific tagging logic, but coordinates fit here
    , CityTarget 546 "Volgograd"        (48.40, 44.30, 48.90, 44.70)
    ]