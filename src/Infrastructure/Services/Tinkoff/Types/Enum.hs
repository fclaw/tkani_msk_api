{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-} -- To automatically derive Generic

module Infrastructure.Services.Tinkoff.Types.Enum where


import Data.Aeson (ToJSON)
import Data.Aeson.TH
import GHC.Generics (Generic)
import Data.Char (toLower)

import Text (camelToSnake, recordLabelModifier)



-- | Налоговая ставка (Tax Rate)
data Tax = None | VAT0 | VAT10 | VAT20 | VAT110 | VAT120
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = map toLower, sumEncoding = UntaggedValue } ''Tax)

-- | Система налогообложения (Taxation System)
data Taxation = OSN | USNIncome | USNIncomeOutcome | ENVD | ESN | Patent
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''Taxation)

-- | Признак предмета расчета (Payment Object)
data PaymentObject = Commodity | Service | Job | Payment
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''PaymentObject)

-- | Признак способа расчета (Payment Method)
data PaymentMethod = FullPayment | FullPrepayment | Prepayment | Advance | Credit
  deriving (Show, Eq)

$(deriveJSON defaultOptions { constructorTagModifier = camelToSnake, sumEncoding = UntaggedValue } ''PaymentMethod)