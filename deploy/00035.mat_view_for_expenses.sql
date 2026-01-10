-- Deploy tkani-api:00035.mat_view_for_expenses to pg

BEGIN;

-- XXX Add DDLs here.
-- migration_create_daily_expenses_summary.sql

CREATE MATERIALIZED VIEW daily_expenses_summary AS
SELECT
    -- 1. Primary Dimension: The specific day of the expense
    e.day AS expense_day,
    
    -- 2. Secondary Dimension: The payer
    COALESCE(p.name, 'Company') AS payer_name,
    
    -- --- Aggregate Metrics ---
    
    -- Total amount spent for that day by that payer
    SUM(e.amount) AS total_amount,
    
    -- Total number of transactions
    COUNT(e.id) AS transaction_count

FROM
    expenses AS e
-- LEFT JOIN to partners to include company expenses
LEFT JOIN
    partners AS p ON e.paid_by_partner_id = p.id
WHERE
    e.day IS NOT NULL

GROUP BY
    -- Group by both dimensions
    e.day,
    payer_name
ORDER BY
    e.day DESC,
    payer_name ASC;

-- Add a UNIQUE INDEX to allow for CONCURRENT refreshing
CREATE UNIQUE INDEX ON daily_expenses_summary (expense_day, payer_name);


COMMIT;
