{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic

module Domain.Warehouse.Enums (FabricLifecycle (..)) where

import Data.Aeson.TH
import Data.Aeson (defaultOptions, constructorTagModifier, sumEncoding, SumEncoding( UntaggedValue ), FromJSON(..), ToJSON(..), Value(String))
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first) -- For cleanly mapping over the 'Left' case of an Either


import Text (camelToSnake)


data FabricLifecycle
  = NewArrival
  | Advertised
  | Regular
  | OnSale
  | Clearance
  | Archived
  deriving (Show, Eq, Generic)


-- The Template Haskell that generates your FromJSON and ToJSON instances
$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''FabricLifecycle)

-- | Implementation for parsing a URL piece into FabricLifecycle.
--   This allows Servant to understand things like "...?status=on_sale"
instance FromHttpApiData FabricLifecycle where
  parseUrlPiece piece =
    let
      -- 1. Aeson's parsers work on a 'Value'. Since we want to parse
      --    a string from the URL, we wrap the input Text in Aeson's
      --    'String' value constructor.
      jsonValue = String piece

      -- 2. 'parseEither' runs a parser (in this case, our existing 'fromJSON' instance)
      --    on a 'Value' and returns a standard 'Either String a'.
      parseResult = parseEither parseJSON jsonValue

    in
      -- 3. The final result needs to be 'Either Text a'. We use 'first' to
      --    map 'T.pack' over the error string if parsing fails.
      first T.pack parseResult

-- | Implementation for converting FabricLifecycle into a URL piece.
--   This is the reverse, used for generating links.
instance ToHttpApiData FabricLifecycle where
  toUrlPiece lifecycle =
    -- 1. Use the existing ToJSON instance to convert our type to a Value.
    --    This will produce a 'Value' of the form 'String "new_arrival"'.
    case toJSON lifecycle of
      -- 2. Safely extract the Text from the 'String' constructor.
      String t -> t
      -- 3. This fallback should never happen for this type, but is good practice.
      _        -> T.pack . show $ lifecycle