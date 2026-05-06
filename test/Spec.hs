import Domain.Logic.BonusCalculator (calculate, TransactionResult(..))
import Data.Int (Int64, Int32)

-- The TransactionResult definition should be in src/Domain.Logic.BonusCalculator.hs 
-- along with calculate

main :: IO ()
main = do
    putStrLn "\n--- Testing Scale: 0 | 1 | 3 | 5 | 7 ---"

    -- Edge: Just below the first floor (499 RUB = 49,900 kopeks)
    -- Rate: 0%
    assertBonus "Amount 499.00 (0%)" 49900 0 0 0

    -- Boundary: The 1% start (500 RUB = 50,000 kopeks)
    -- Math: (50,000 * 1) / 10,000 = 5 points
    assertBonus "Amount 500.00 (1%)" 50000 0 0 5

    -- Boundary: The 2% start (1000 RUB = 100,000 kopeks)
    -- Math: (100,000 * 2) / 10,000 = 20 points
    assertBonus "Amount 1,000.00 (2%)" 100000 0 0 20

    -- Boundary: The 3% start (5,000 RUB = 500,000 kopeks)
    -- Math: (500,000 * 3) / 10,000 = 150 points
    assertBonus "Amount 5,000.00 (3%)" 500000 0 0 150

    -- Boundary: The 7% VIP start (15,000 RUB = 1,500,000 kopeks)
    -- Math: (1,500,000 * 5) / 10,000 = 750 points
    assertBonus "Amount 15,000.00 (5%)" 1500000 0 0 750

    -- Real World: Median Sale (26,500 RUB = 2,650,000 kopeks)
    -- Math: (2,650,000 * 5) / 10,000 = 1,325 points
    assertBonus "Median Sale 26.5k (5%)" 2650000 0 0 1325

    -- Real World: The Whale (108,000 RUB = 10,800,000 kopeks)
    -- Math: (10,800,000 * 5) / 10,000 = 5,400 points
    assertBonus "Whale Sale 108k (5%)" 10800000 0 0 5400

    -- Complexity Check: 30,000 RUB buy (3,000,000 kopeks), using 10,000 points.
    -- Cash: 3,000,000 - (10,000 * 100) = 2,000,000 kopeks.
    -- Rate: 5% (determined by total order size 3,000,000).
    -- Added Points: (2,000,000 * 5) / 10,000 = 1000 points.
    -- Balance: 10,000 (bal) - 10,000 (used) + 1000 (earned) = 1000.
    -- assertBonus "Mixed Pay: 30k buy, 10k use" 3000000 10000 10000 1000

    putStrLn "--- All Tests Completed ---\n"

assertBonus :: String -> Int64 -> Int32 -> Int32 -> Int32 -> IO ()
assertBonus label amt bal req expected = do
    let res = calculate amt bal req
    if newBalance res == expected
        then putStrLn $ " [OK]   " ++ label
        else putStrLn $ " [FAIL] " ++ label ++ " -> Got " ++ show (newBalance res) ++ " (Expected " ++ show expected ++ ")"