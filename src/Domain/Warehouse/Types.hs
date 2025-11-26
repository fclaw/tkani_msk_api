{-# LANGUAGE DeriveGeneric #-}

module Domain.Warehouse.Types where

import Data.Text (Text)
import GHC.Generics (Generic)

data FabricType = Roll | PreCut
  deriving (Show, Eq, Generic)

data Fabric = Fabric
  { fName        :: Text
  , fPrice       :: Int -- either for per meter or total price
  , fArticle     :: Text
  , fDescription :: Text -- Includes composition and hashtags
  , fType        :: FabricType
  , fLength      :: Maybe Double -- Only for PreCut
  } deriving (Show, Eq)

data AdminParseError
  -- | Structure Errors (e.g. Empty post or too few lines)
  = StructureError FabricType Text 
  
  -- | Value Errors (e.g. "Text" instead of "123")
  | InvalidPrice Text    -- Stores the bad text
  | InvalidLength Text   -- Stores the bad text
  | ValueError FabricType Text
  | AmbiguousFormat Text -- New constructor for generic failures
  | InvalidArticleFormat Text -- New constructor
  deriving (Show, Eq)