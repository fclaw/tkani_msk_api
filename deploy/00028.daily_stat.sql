-- Deploy tkani-api:00026.daily_stat to pg

BEGIN;

-- XXX Add DDLs here.
-- migration_create_daily_sales_stats.sql

DROP MATERIALIZED VIEW IF EXISTS daily_sales_stats;
CREATE MATERIALIZED VIEW daily_sales_stats AS
SELECT
    -- The date dimension
    CAST(o.created_at AT TIME ZONE 'Europe/Moscow' AS date) AS sale_date,
    
    -- --- Sales Volume Metrics ---
    COUNT(DISTINCT o.id) AS total_orders,
    -- Count of all distinct items sold (rolls + pre-cuts)
    COUNT(oi.order_id) AS total_line_items_sold, 
    
    -- --- Financial Metrics ---
    SUM(oi.total_price_kopecks) / 100.0 AS total_revenue,
    
    -- --- Product Metrics ---
    COUNT(DISTINCT oi.fabric_id) AS unique_fabrics_sold,
    jsonb_agg(DISTINCT oi.fabric_name) FILTER (WHERE oi.fabric_name IS NOT NULL) AS fabric_names,
    
    -- --- Breakdown by Type ---
    COUNT(*) FILTER (WHERE oi.is_pre_cut) AS pre_cuts_sold_count,
    COUNT(*) FILTER (WHERE NOT oi.is_pre_cut AND oi.length_m IS NOT NULL) AS rolls_sold_count,
    SUM(oi.length_m) FILTER (WHERE NOT oi.is_pre_cut) AS total_meters_sold
    
FROM
    orders AS o
JOIN
    -- A subquery to unify and pre-calculate all line items
    (
        -- Part 1: Items from standard, inventoried fabrics
        SELECT 
            ofb.order_id,
            ofb.fabric_id,
            f.name AS fabric_name,
            ofb.pre_cut_id,
            ofb.length_m,
            (ofb.pre_cut_id IS NOT NULL) AS is_pre_cut,

            -- THE CORRECTED PRICE CALCULATION
            COALESCE(
                -- If it's a pre-cut, use its price
                pc.price_rub,
                -- If it's a roll, calculate price_per_meter * length
                f.price_per_meter * ofb.length_m
            ) * 100 AS total_price_kopecks -- Multiply by 100 to work with integers (kopecks)

        FROM order_fabric_bindings AS ofb
        -- Join to fabrics to get name and price/meter
        LEFT JOIN fabrics AS f ON ofb.fabric_id = f.id
        -- Join to pre_cuts to get its price
        LEFT JOIN pre_cuts AS pc ON ofb.pre_cut_id = pc.id

        UNION ALL

        -- Part 2: Items from manually created YAML orders
        SELECT
            moi.order_id,
            NULL AS fabric_id,
            moi.item_name AS fabric_name,
            NULL AS pre_cut_id,
            moi.length_m,
            (moi.fabric_type = 'pre_cut') AS is_pre_cut,
            
            -- Assuming total_price is in rubles here
            moi.total_price * 100 AS total_price_kopecks
            
        FROM manual_order_items AS moi
    ) AS oi ON o.id = oi.order_id
    
WHERE
     o.status != 'registered'
    AND o.status != 'cancelled'
    AND o.is_removed_from_delivery_provider = FALSE
GROUP BY
    sale_date
ORDER BY
    sale_date DESC;

-- Don't forget the unique index for concurrent refresh
CREATE UNIQUE INDEX ON daily_sales_stats (sale_date);

COMMIT;
