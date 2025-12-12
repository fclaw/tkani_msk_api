{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Warehouse.Types where

import Data.Text (Text, toLower)
import Data.Aeson.TH
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData(..))

import Text (camelToSnake) 

-- The target data structure we are parsing into.
data Fabric = Fabric
  { fName        :: Text
  , fPrice       :: Int
  , fArticle     :: Text
  , fDescription :: Text
  , fType        :: FabricType
  , fLength      :: Double
  , fWidth       :: Int
  , fIsSearchable :: Bool
  } deriving (Show, Eq)

data FabricType = Roll | PreCut
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''FabricType)

instance FromHttpApiData FabricType where
  parseUrlPiece text =
    -- We'll make it case-insensitive for robustness
    case toLower text of
      "roll"     -> Right Roll
      "precut"   -> Right PreCut
      _          -> Left "Unknown fabric type"

-- All possible validation errors.
data ParseError
  = StructureError FabricType Text
  | ValueError FabricType Text
  | InvalidArticleFormat Text
  | AmbiguousFormat Text -- For when we can't even guess
  | MissingVisibilityTag
  deriving (Show, Eq)