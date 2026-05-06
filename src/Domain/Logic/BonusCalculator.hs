module Domain.Logic.BonusCalculator (calculate, TransactionResult (..)) where

import Data.Int (Int64, Int32)

-- A Record to represent the result of the transaction
data TransactionResult = 
     TransactionResult 
     { netAmountToPay :: Int64,
       pointsUsed     :: Int32,
       pointsEarned   :: Int32,
       newBalance     :: Int32
     } deriving (Show)

-- | calculate logic
-- amount: price of fabrics
-- currentBalance: user's loyalty points before sale
-- requestedToSpend: how many points the customer WANTS to use
--   Condition: Expended points cannot exceed 50% of the original Amount.
--   Progressive Earning Tier logic:
--   - <= 999    : 1%
--   - <= 4,999. : 2%
--   - <= 14,999 : 3%
--   - > 15,000  : 5%
calculate :: Int64 -> Int32 -> Int32 -> TransactionResult
calculate amountKopeks currentBalanceRub requestedToSpendRub =
    let
        -- 1. Actual points used is the MINIMUM of:
        --    - What they asked for
        --    - What they actually have
        --    - 30% of the bill
        -- Constraints (Working with Kopeks)
        -- maxSpendable translates RUB points to kopeks (multiply by 100) 
        -- but simpler: Max RUB spend is (Amount_in_kopeks / 100) / 10
        maxPointsPossible = fromIntegral $ (amountKopeks `div` 100) `div` 10
        actualExpended = minimum [requestedToSpendRub, currentBalanceRub, maxPointsPossible]

        -- 2. What they actually pay at the register
          -- Payment in kopeks
        cashPaidKopeks  = amountKopeks - (fromIntegral actualExpended * 100)

        -- 3. Calculate how much they EARN (Progressive scale)
        -- Logic: Earned on the CASH portion only
        rate | amountKopeks < 50000       = 0 -- 0% for < 500 RUB
             | amountKopeks >= 50000 && 
               amountKopeks < 100000      = 1 -- 1% for 500 - 999 RUB
             | amountKopeks >= 100000 && 
               amountKopeks < 500000      = 2 -- 2% for 1,000 - 4,999 RUB
             | amountKopeks >= 500000 && 
               amountKopeks < 1500000     = 3 -- 3% for 5,000 - 14,999 RUB
             | otherwise                  = 5 -- 5% for 15,000 RUB and above
             
        -- Math: (Kopeks * Rate) / 10,000 = Whole RUB points
        -- Example: (200,000 kopeks * 5) / 10,000 = 100 points
        pointsAdded = fromIntegral $ (cashPaidKopeks * rate) `div` 10000

        -- 4. Final Balance calculation
        finalBalance = currentBalanceRub - actualExpended + pointsAdded

    in TransactionResult cashPaidKopeks (fromIntegral actualExpended) (fromIntegral pointsAdded) (fromIntegral finalBalance)