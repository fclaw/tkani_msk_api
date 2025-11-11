-- We need these extensions for our custom 'ToJSON' instance
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- We need this to get the field name from the type level to the value level
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module API.WithField where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), withObject, (.:))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy
import Data.Aeson.KeyMap
import Data.String (fromString)



-- | A generic wrapper that adds a field with a given name ('fieldName')
--   and value ('fieldValue') to an existing data type ('a').
--
--   The 'fieldName' is a type-level string (Symbol).
data WithField (fieldName :: Symbol) idType a = WithField
  { withFieldValue :: idType
  , withFieldData  :: a
  } deriving (Show)

-- | The magic: our custom ToJSON instance.
instance (KnownSymbol fieldName, ToJSON idType, ToJSON a) => ToJSON (WithField fieldName idType a) where
  toJSON (WithField fieldValue innerData) =
    -- 1. Get the name of the field from the type level.
    --    'symbolVal' brings a type-level string down to the value level.
    let fieldNameStr = symbolVal (Proxy :: Proxy fieldName)
    
    -- 2. Convert the inner data 'a' into its JSON representation (which will be an object).
    in case toJSON innerData of
        -- 3. Check if the inner JSON is an object.
        Object obj ->
          -- 4. If it is, insert our new field into that object.
          Object (insert (fromString fieldNameStr) (toJSON fieldValue) obj)
        
        -- 5. If the inner JSON is not an object (e.g., a String or Number),
        --    we cannot inject a field. We'll return an error object.
        _ -> object ["error" .= ("WithField can only be used on types that encode to a JSON object." :: String)]

-- | The new 'FromJSON' instance
instance (KnownSymbol fieldName, FromJSON idType, FromJSON a) => FromJSON (WithField fieldName idType a) where
  parseJSON = withObject "WithField" $ \obj -> do
    -- 1. Get the field name from the type level.
    let fieldNameStr = fromString $ symbolVal (Proxy :: Proxy fieldName)
    
    -- 2. Parse the injected field's value (e.g., the 'id') from the object.
    --    The '.:' operator looks up a key and parses its value.
    fieldValue <- obj .: fieldNameStr
    
    -- 3. Parse the inner data type 'a' from the same object.
    --    Aeson is smart enough to use the 'FromJSON' instance for 'a' (e.g., 'FabricInfo'),
    --    which will parse all of its own fields.
    innerData <- parseJSON (Object obj)

    -- 4. If both parsing steps succeed, construct the 'WithField' value.
    return $ WithField fieldValue innerData

-- | A convenient type alias for the common case of adding an "id" field.
type WithId idType a = WithField "id" idType a