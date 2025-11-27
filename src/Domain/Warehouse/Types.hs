{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic
{-# LANGUAGE TemplateHaskell   #-}

module Domain.Warehouse.Types where

import Data.Text (Text)
import Data.Aeson.TH
import GHC.Generics (Generic)

import Text (camelToSnake) 

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
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''FabricType)

-- All possible validation errors.
data AdminParseError
  = StructureError FabricType Text
  | ValueError FabricType Text
  | InvalidArticleFormat Text
  | AmbiguousFormat Text -- For when we can't even guess
  deriving (Show, Eq)