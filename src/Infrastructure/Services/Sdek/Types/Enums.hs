{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Infrastructure.Services.Sdek.Types.Enums where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson.Types (withScientific) -- Also useful, but we'll use a direct pattern match
import Data.Scientific (toBoundedInteger)


-- | Represents the SDEK VAT Rate codes.
data SdekVatRate
  = VatRate0
  | VatRate5
  | VatRate7
  | VatRate10
  | VatRate16
  | VatRate22
  | NoVat -- Represents the 'null' case
  deriving (Show, Eq, Generic)

-- The Corrected instance
instance FromJSON SdekVatRate where
  -- 'parseJSON' is the function we are defining. It takes one argument.
  -- We'll name that argument 'value'.
  parseJSON value = 
    -- Now, we pattern match on this 'value'.
    case value of
      -- Case 1: The value is a JSON Number
      Number n ->
        -- 'toBoundedInteger' safely converts the 'Scientific' number to an 'Int'
        case toBoundedInteger @Int n of
          Just 0  -> pure VatRate0
          Just 5  -> pure VatRate5
          Just 7  -> pure VatRate7
          Just 10 -> pure VatRate10
          Just 16 -> pure VatRate16
          Just 22 -> pure VatRate22
          -- If it's a number, but not one of our codes
          _       -> fail ("Invalid integer value for SdekVatRate: " ++ show n)
      
      -- Case 2: The value is JSON Null
      Null ->
        pure NoVat
        
      -- Case 3: The value is any other type (String, Object, Array, etc.)
      _ ->
        fail ("Expected a Number or Null for SdekVatRate, but got: " ++ show value)


-- | Custom JSON serializer for SdekVatRate
instance ToJSON SdekVatRate where
  toJSON VatRate0  = Number 0
  toJSON VatRate5  = Number 5
  toJSON VatRate7  = Number 7
  toJSON VatRate10 = Number 10
  toJSON VatRate16 = Number 16
  toJSON VatRate22 = Number 22
  toJSON NoVat     = Null


vatToDouble :: SdekVatRate -> Double
vatToDouble vatRate =
  case vatRate of
    VatRate0  -> 0
    VatRate5  -> 5
    VatRate7  -> 7
    VatRate10 -> 10
    VatRate16 -> 16
    VatRate22 -> 22
    NoVat     -> 0