{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic

module Infrastructure.Services.Tinkoff.Types.Enum where


import Data.Aeson.TH
import Data.Aeson (defaultOptions, SumEncoding(..))
import GHC.Generics (Generic)
import Data.Char (isUpper, toLower) -- for our helper

import Infrastructure.Services.Tinkoff.Types.Enum.Modifier 


data Tax = None | VAT0 | VAT5 | VAT7 | VAT10 | VAT20 | VAT105 | VAT107 | VAT110 | VAT120
  deriving (Show, Eq)

-- For 'Tax', the conversion is simpler: just lowercase. 'None' -> "none", 'VAT0' -> "vat0"
$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''Tax)


data Taxation = 
      -- OSN (General Taxation System / ОСН): The default, full tax regime. 
      -- Includes Value Added Tax (VAT/NDS) and Income/Profit tax. Used by large businesses.
      OSN
      -- USNIncome (Simplified: Income / УСН Доходы): 
      -- Simplified regime where tax is paid on total revenue (usually 6%). Expenses are ignored 
    | USNIncome
      -- USNIncomeOutcome (Simplified: Income - Expenses / УСН Доходы минус Расходы): 
      -- Simplified regime where tax is paid on the net profit (Revenue minus Expenses), usually at 15%.
    | USNIncomeOutcome
     -- ENVD (Single Tax on Imputed Income / ЕНВД): A flat tax based on physical indicators 
     -- (floor space, headcount) rather than actual revenue. 
     -- (Note: Officially abolished in Russia as of 2021, but exists in legacy code).
    | ENVD
      -- ESN (Unified Agricultural Tax / ЕСХН): 
      -- A special tax regime specifically for agricultural producers and farmers.
    | ESN
      -- Patent (Patent System / ПСН): A fixed-fee license system where a merchant buys a "Patent" for a specific activity for a set period of time 
    | Patent
  deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = modifier, sumEncoding = UntaggedValue } ''Taxation)


data PaymentObject = Commodity | Service | Job | Payment
  deriving (Show, Eq)

-- This just needs simple lowercasing.
$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''PaymentObject)


data PaymentMethod = FullPayment | FullPrepayment | Prepayment | Advance | Credit
  deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = modifier, sumEncoding = UntaggedValue } ''PaymentMethod)

-- | Specifies the desired format for the QR code data.
data QrDataType = PAYLOAD | IMAGE 
  deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = id, sumEncoding = UntaggedValue } ''QrDataType)