{-# LANGUAGE DeriveGeneric #-}

module Domain.Warehouse.Types where

import Data.Text (Text)
import GHC.Generics (Generic)

data FabricType = Roll | PreCut
  deriving (Show, Eq, Generic)

data ParsedFabric = ParsedFabric
  { pfName        :: Text
  , pfPrice       :: Int
  , pfArticle     :: Text
  , pfDescription :: Text -- Includes composition and hashtags
  , pfType        :: FabricType
  , pfLength      :: Maybe Double -- Only for PreCut
  } deriving (Show, Eq)

  -- Updated Error Type: Carries the FabricType context
data AdminParseError
  = StructureError FabricType Text  -- e.g. "Not enough lines"
  | ValueError FabricType Text      -- e.g. "Price is text, not number"
  | UnknownType                     -- Couldn't detect #tags
  deriving (Show, Eq)