-- In a new file, e.g., Domain/Services/Dostavista/Types/Error.hs

{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Services.Dostavista.Types.Error where


import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import Data.Aeson.TH (fieldLabelModifier)
import Data.Aeson (FromJSON(..), genericParseJSON, defaultOptions)

import Text (recordLabelModifier)


-- | Represents the specific validation errors for each parameter.
--   The keys of the map are parameter names (e.g., "points").
--   The values are a list of error codes (e.g., ["min_size"]).
type ParameterErrors = Map Text [Text]

-- | The top-level error response from the Dostavista API.
data DostavistaErrorResponse = 
     DostavistaErrorResponse
     { derIsSuccessful    :: Bool
     , derErrors          :: [Text]
     , derParameterErrors :: ParameterErrors
     } deriving (Show, Eq, Generic)

instance FromJSON DostavistaErrorResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = recordLabelModifier "der" }