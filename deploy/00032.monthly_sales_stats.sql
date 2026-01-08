-- Deploy tkani-api:00032.monthly_sales_stats to pg

BEGIN;

-- XXX Add DDLs here.
-- migration_create_monthly_sales_stats.sql

CREATE MATERIALIZED VIEW monthly_sales_stats AS
SELECT
    -- 1. The primary dimension: the month.
    --    to_char() is a powerful formatting function. 'YYYY-MM' gives us a sortable key like "2025-12".
    to_char(sale_date, 'YYYY-MM') AS sale_month,
    
    -- --- Aggregate Metrics (Calculated from the daily stats) ---
    
    -- A. Total orders for the entire month
    SUM(total_orders) AS total_monthly_orders,
    
    -- B. Average orders per day for that month
    --    We use AVG() on the daily total to get the average.
    ROUND(AVG(total_orders), 2) AS average_orders_per_day,
    
    -- C. Total revenue for the entire month
    SUM(total_revenue) AS total_monthly_revenue,

    -- D. Average revenue per day for that month
    AVG(total_revenue) AS average_revenue_per_day

    /* 
       PROFIT CALCULATION:
       Assuming your Gross Profit Margin is 60% (based on our earlier discussion).
       We can apply this margin to the revenue figures.
    */
    ,
    (SUM(total_revenue) * 0.60) AS total_estimated_profit,
    (AVG(total_revenue) * 0.60) AS average_estimated_profit_per_day
    
FROM
    -- We query our FAST, pre-calculated daily view, not the slow raw tables.
    daily_sales_stats

GROUP BY
    sale_month
ORDER BY
    sale_month DESC;

-- Add a unique index for concurrent refreshing
CREATE UNIQUE INDEX ON monthly_sales_stats (sale_month);


COMMIT;
