-- Deploy tkani-api:00036.mat_view_for_expenses to pg

BEGIN;

-- XXX Add DDLs here.
CREATE MATERIALIZED VIEW monthly_expenses_summary AS
SELECT
    -- 1. The primary dimension: the month.
    to_char(expense_day, 'YYYY-MM') AS expense_month,
    
    -- 2. The Grand Total expense for the entire month (sum of all payers)
    SUM(total_amount) AS total_monthly_expenses,
    
    -- 3. The nested array of expenses by payer
    array_agg(
        jsonb_build_object(
            'payer', payer_name,
            'amount', total_amount,
            'transactions', transaction_count
        )
        -- Order the payers alphabetically within the JSON array for consistency
        ORDER BY payer_name ASC
    ) AS expenses_by_payer
    
FROM
    -- We still query from the fast daily_expenses_summary view.
    daily_expenses_summary

GROUP BY
    expense_month
ORDER BY
    expense_month DESC;

-- Add the unique index for concurrent refreshing
CREATE UNIQUE INDEX ON monthly_expenses_summary (expense_month);


COMMIT;
