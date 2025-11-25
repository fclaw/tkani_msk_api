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
  = MissingArticle
  | MissingPrice
  | InvalidPriceFormat Text
  | MissingName
  | InvalidLengthFormat Text
  deriving (Show, Eq)