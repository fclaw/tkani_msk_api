{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Services.Reporting (generateAndSendDailyReport) where

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

import App (AppM, ChatKey (ORDER), _appDBPool)
import Infrastructure.Database (refreshAndFetchDailyStats)
import qualified Infrastructure.Services.Telegram as Telegram
import Utils.Telegram.Markdown (escapeMarkdownV2)

-- 1. ADT for a single row of the CSV report
data DailyReportRow = DailyReportRow
  { rptSaleDate       :: Text
  , rptTotalOrders    :: Int32
  , rptTotalRevenue   :: Double
  , rptPreCutsSold    :: Int32
  , rptRollsSold      :: Int32
  , rptTotalMetersSold :: Double
  } deriving (Generic)

-- 2. CSV Header and encoding instances
instance DefaultOrdered DailyReportRow where
  headerOrder _ = V.fromList
    [ "sale_date", "total_orders", "total_revenue"
    , "pre_cuts_sold", "rolls_sold", "total_meters_sold"
    ]

instance ToNamedRecord DailyReportRow where
  toNamedRecord r = namedRecord
    [ "sale_date"         .= rptSaleDate r
    , "total_orders"      .= rptTotalOrders r
    , "total_revenue"     .= rptTotalRevenue r
    , "pre_cuts_sold"     .= rptPreCutsSold r
    , "rolls_sold"        .= rptRollsSold r
    , "total_meters_sold" .= rptTotalMetersSold r
    ]


-- 3. The Main Service Function
generateAndSendDailyReport :: AppM ()
generateAndSendDailyReport = do
  $(logTM) InfoS "Starting daily sales report generation..."
  -- Step 1: Refresh the materialized view and fetch the data
  pool <- fmap _appDBPool ask
  eStats <- refreshAndFetchDailyStats pool
  case eStats of
    Left dbErr -> $(logTM) ErrorS $ "Failed to fetch daily stats from DB: " <> ls (show dbErr)
    Right stats -> do
      -- Step 2: Convert the stats to CSV format
     -- We need to convert Day to Text for CSV
            let reportRows = map toReportRow stats
            let csvData = encodeDefaultOrderedByName reportRows
            todayStr <- (T.pack . formatTime defaultTimeLocale "%Y-%m-%d") <$> (liftIO getZonedTime)
            let filename = "daily_sales_report_" <> todayStr <> ".csv"
            let caption = "📈 Ежедневный отчет по продажам за " <> escapeMarkdownV2 todayStr

            eRes <- Telegram.sendDocument ORDER caption filename (BL.toStrict csvData) "text/csv"
            case eRes of
              Left err -> $(logTM) ErrorS $ "Failed to send daily sales report: " <> ls (show err)
              Right _ -> $(logTM) InfoS "Successfully generated and sent daily sales report."

-- Helper to convert the DB row type to the CSV row type
toReportRow :: (Day, Int32, Double, Int32, Int32, Maybe Double) -> DailyReportRow
toReportRow (date, orders, revenue, precuts, rolls, meters) =
  DailyReportRow
    { rptSaleDate = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" date
    , rptTotalOrders = orders
    , rptTotalRevenue = revenue
    , rptPreCutsSold = precuts
    , rptRollsSold = rolls
    , rptTotalMetersSold = fromMaybe 0.0 meters
    }