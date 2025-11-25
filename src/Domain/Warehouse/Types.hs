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

data AdminParseError
  -- | Structure Errors (e.g. Empty post or too few lines)
  = NotEnoughLines FabricType Int 
  
  -- | Value Errors (e.g. "Text" instead of "123")
  | InvalidPrice Text    -- Stores the bad text
  | InvalidLength Text   -- Stores the bad text
  
  -- | Type Logic
  | MissingTag

  -- | General fallback if regex/other checks fail
  | UnknownFormat
  deriving (Show, Eq)