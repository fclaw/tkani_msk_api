{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic


module Infrastructure.Services.Types where

import Data.Aeson (ToJSON)
import Data.Aeson.TH
import GHC.Generics (Generic)
import Data.Char (toLower)


-- This module can be used to re-export common types used across various services.
-- For example, if multiple services share certain data types, you can define them here
-- and then import this module in those services to avoid duplication.

data PaymentProvider = Tinkoff | None
  deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''PaymentProvider)

--  'pending',     Link created, waiting for user to pay
-- 'confirmed',    Payment successful
-- 'rejected',     Payment failed (e.g., card declined)
-- 'cancelled',    Payment cancelled by user or timeout
-- 'error'         An unexpected error occurred
data PaymentStatus = 
     Pending
    | Confirmed
    | Rejected
    | Cancelled
    | Error
  deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''PaymentStatus)