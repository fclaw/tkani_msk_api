{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module Domain.Services.Reporting 
       ( generateAndSendDailyReport
       , generateAndSendMonthlyReport) 
       where

import Data.Csv (ToNamedRecord(..), DefaultOrdered(..), namedRecord, (.=), encodeDefaultOrderedByName)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Katip (logTM, ls, Severity(InfoS, ErrorS))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
import Control.Monad.Reader.Class (ask)
import Data.Int (Int32)
import Data.Time.Calendar.Month (Month(..)) -- Import the constructor
import Data.Time (Day, fromGregorian, formatTime, defaultTimeLocale)
import Data.Bifunctor (first)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)


import App (AppM, ChatKey (ORDER), _appDBPool, extractFromEither)
import Infrastructure.Database 
       ( extractADT
       , refreshAndFetchDailyStats
       , refreshAndFetchMonthlyStats
       , DailyExpensesStat (..))
import qualified Infrastructure.Database as Db (MonthlyStat(..)) 
import qualified Infrastructure.Services.Telegram as Telegram
import Utils.Telegram.Markdown (escapeMarkdownV2)

-- 1. ADT for a single row of the CSV report
data DailyReportRow = DailyReportRow
  { rptSaleDate        :: Text
  , rptTotalOrders     :: Int32
  , rptTotalRevenue    :: Double
  , rptPreCutsSold     :: Int32
  , rptRollsSold       :: Int32
  , rptTotalMetersSold :: Double
  , rptExpenses        :: Text
  , rptNetIncome       :: Double
  } deriving (Generic)

-- 2. CSV Header and encoding instances
instance DefaultOrdered DailyReportRow where
  headerOrder _ = V.fromList
    [ "sale_date"
    , "total_orders"
    , "total_revenue"
    , "pre_cuts_sold"
    , "rolls_sold"
    , "total_meters_sold"
    , "expenses"
    , "net_income"
    ]

instance ToNamedRecord DailyReportRow where
  toNamedRecord r = namedRecord
    [ "sale_date"         .= rptSaleDate r
    , "total_orders"      .= rptTotalOrders r
    , "total_revenue"     .= rptTotalRevenue r
    , "pre_cuts_sold"     .= rptPreCutsSold r
    , "rolls_sold"        .= rptRollsSold r
    , "total_meters_sold" .= rptTotalMetersSold r
    , "expenses"          .= rptExpenses r
    , "net_income"        .= rptNetIncome r
    ]


-- 3. The Main Service Function
generateAndSendDailyReport :: AppM ()
generateAndSendDailyReport = do
  $(logTM) InfoS "Starting daily sales report generation..."
  -- Step 1: Refresh the materialized view and fetch the data
  pool <- fmap _appDBPool ask
  let day = fromOrdinalDate 2026 1
  eStats <- refreshAndFetchDailyStats day pool
  extractFromEither eStats $ \stats -> do
    -- Step 2: Convert the stats to CSV format
    -- We need to convert Day to Text for CSV
    let eReportRows = sequence $ map toReportRow stats
    extractFromEither eReportRows $ \reportRows -> do

      let csvData = encodeDefaultOrderedByName reportRows
      todayStr <- (T.pack . formatTime defaultTimeLocale "%Y-%m-%d") <$> (liftIO getZonedTime)
      let filename = "daily_sales_expenses_report_" <> todayStr <> ".csv"
      let caption = "📈 Daily sales and expenses report for " <> escapeMarkdownV2 todayStr

      eRes <- Telegram.sendDocument ORDER caption filename (BL.toStrict csvData) "text/csv"
      case eRes of
        Left err -> $(logTM) ErrorS $ "Failed to send daily sales and expenses report: " <> ls (show err)
        Right _ -> $(logTM) InfoS "Successfully generated and sent daily sales and expenses report."

-- Helper to convert the DB row type to the CSV row type
toReportRow :: (Day, Int32, Double, Int32, Int32, Maybe Double, Either Text [DailyExpensesStat]) -> Either Text DailyReportRow
toReportRow (date, orders, revenue, precuts, rolls, meters, eExpenses) =
  flip fmap eExpenses $ \expenses ->
    let
        totalExpenses = sum $ map desAmount expenses
        -- This is the key logic: convert the list of expenses into a single string
        expensesString = T.intercalate "; " (map formatExpense expenses)
        formatExpense :: DailyExpensesStat -> Text
        formatExpense exp = desPayer exp <> ": " <> (T.pack $ show $ desAmount exp) <> " RUB"
    in DailyReportRow
       { rptSaleDate = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" date
       , rptTotalOrders = orders
       , rptTotalRevenue = revenue
       , rptPreCutsSold = precuts
       , rptRollsSold = rolls
       , rptTotalMetersSold = fromMaybe 0.0 meters
       , rptExpenses = expensesString
       , rptNetIncome = revenue - totalExpenses
       }

-- | Formats a 'Month' type into a human-readable "MonthName YYYY" string.
--
--   Args:
--     month: The Month value to format.
--
--   Returns:
--     A Text string, e.g., "December 2025" or "Декабрь 2025".
formatMonth :: Month -> Text
formatMonth (MkMonth monthInteger) =
        -- 1. Extract the Year and Month-of-Year from the absolute integer value
    let
        year = (monthInteger - 1) `div` 12
        month = fromIntegral $ (monthInteger - 1) `mod` 12 + 1 -- month is 1-12
        -- 2. Create a dummy 'Day' object to use with formatTime.
        --    We just need the year and month; the day (1) is arbitrary.
        dummyDay :: Day = fromGregorian year month 1
    
    -- 3. Use formatTime with the correct format specifiers.
    --    '%B' gives the full month name (e.g., "December").
    --    '%Y' gives the 4-digit year.
    in T.pack $ formatTime defaultTimeLocale "%B %Y" dummyDay



data MonthlyStat = 
     MonthlyStat
     { msSaleMonth       :: Month
     , msTotalOrders     :: Int32
     , msAvgOrdersPerDay :: Int32
     , msTotalProfit     :: Double
     , msAvgProfitPerDay :: Double
     , msTotalExpenses   :: Double
     , msPayersExpenses  :: Text
     , msNetIncome       :: Double
     } deriving (Show)

mkMonthlyStat :: Db.MonthlyStat -> MonthlyStat
mkMonthlyStat dbStats =
  let expenses = extractADT $ first T.unpack $ Db.msPayersExpenses dbStats
      expensesString = T.intercalate "; " (map formatExpense expenses)
      formatExpense :: DailyExpensesStat -> Text
      formatExpense exp = desPayer exp <> ": " <> (T.pack $ show $ desAmount exp) <> " RUB"
      totalExpenses = sum $ map desAmount expenses
  in MonthlyStat 
     { msSaleMonth       = Db.msSaleMonth dbStats
     , msTotalOrders     = Db.msTotalOrders dbStats
     , msAvgOrdersPerDay = Db.msAvgOrdersPerDay dbStats
     , msTotalProfit     = Db.msTotalProfit dbStats
     , msAvgProfitPerDay = Db.msAvgProfitPerDay dbStats
     , msTotalExpenses   = Db.msTotalProfit dbStats
     , msPayersExpenses  = expensesString
     , msNetIncome       = Db.msTotalProfit dbStats - totalExpenses
     }

instance DefaultOrdered MonthlyStat where
  headerOrder _ = 
    V.fromList [ 
        "sale_month"
      , "total_orders"
      , "average_orders_per_day"
      , "total_profit"
      , "average_profit_per_day"
      , "total_expenses"
      , "expenses"
      , "net_income"
    ]

instance ToNamedRecord MonthlyStat where
  toNamedRecord r = namedRecord
    [ "sale_month"             .= formatMonth (msSaleMonth r)
    , "total_orders"           .= msTotalOrders r
    , "average_orders_per_day" .= msAvgOrdersPerDay r
    , "total_profit"           .= msTotalProfit r
    , "average_profit_per_day" .= msAvgProfitPerDay r
    , "total_expenses"         .= msTotalExpenses r
    , "expenses"               .= msPayersExpenses r
    , "net_income"             .= msNetIncome r
    ]


generateAndSendMonthlyReport :: AppM ()
generateAndSendMonthlyReport = do
  $(logTM) InfoS "Starting monthly sales and expenses report generation..."
  -- Step 1: Refresh the materialized view and fetch the data
  pool <- fmap _appDBPool ask
  eStats <- refreshAndFetchMonthlyStats pool
  extractFromEither eStats $ \dbStats -> do
    let domainStats = map mkMonthlyStat dbStats
    let csvData = encodeDefaultOrderedByName domainStats
    let filename = "12_months_sales_and_expenses_report_.csv"
    let caption = "📈 Sales and expenses report for the last 12 months"
    eRes <- Telegram.sendDocument ORDER caption filename (BL.toStrict csvData) "text/csv"
    case eRes of
      Left err -> $(logTM) ErrorS $ "Failed to send monthly sales and expenses report: " <> ls (show err)
      Right _ -> $(logTM) InfoS "Successfully generated and sent monthly sales and expenses report."