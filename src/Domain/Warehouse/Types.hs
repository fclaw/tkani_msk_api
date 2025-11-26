module Domain.Warehouse.Types where

import Data.Text (Text)

-- The target data structure we are parsing into.
data Fabric = Fabric
  { fName        :: Text
  , fPrice       :: Int
  , fArticle     :: Text
  , fDescription :: Text
  , fType        :: FabricType
  , fLength      :: Double
  } deriving (Show, Eq)

data FabricType = Roll | PreCut
  deriving (Show, Eq)

-- All possible validation errors.
data AdminParseError
  = StructureError FabricType Text
  | ValueError FabricType Text
  | InvalidArticleFormat Text
  | AmbiguousFormat Text -- For when we can't even guess
  deriving (Show, Eq)