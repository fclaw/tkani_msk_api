{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic

module Infrastructure.Services.Tinkoff.Types.Enum where


import Data.Aeson.TH
import Data.Aeson (defaultOptions, SumEncoding(..))
import GHC.Generics (Generic)
import Data.Char (isUpper, toLower) -- for our helper

import Infrastructure.Services.Tinkoff.Types.Enum.Modifier 



data Tax = None | VAT0 | VAT10 | VAT20 | VAT110 | VAT120
  deriving (Show, Eq, Generic)

-- For 'Tax', the conversion is simpler: just lowercase. 'None' -> "none", 'VAT0' -> "vat0"
$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''Tax)


data Taxation = OSN | USNIncome | USNIncomeOutcome | ENVD | ESN | Patent
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = modifier, sumEncoding = UntaggedValue } ''Taxation)


data PaymentObject = Commodity | Service | Job | Payment
  deriving (Show, Eq, Generic)

-- This just needs simple lowercasing.
$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''PaymentObject)


data PaymentMethod = FullPayment | FullPrepayment | Prepayment | Advance | Credit
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = modifier, sumEncoding = UntaggedValue } ''PaymentMethod)