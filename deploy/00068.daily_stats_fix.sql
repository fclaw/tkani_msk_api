-- Deploy tkani-api:00026.daily_stat to pg

BEGIN;

-- XXX Add DDLs here.
-- migration_create_daily_sales_stats.sql
DROP MATERIALIZED VIEW IF EXISTS daily_sales_stats;

CREATE MATERIALIZED VIEW daily_sales_stats AS
WITH all_revenue_streams AS (
    -- --- ПОТОК 1: Традиционные заказы (из рулонов и нарезок) ---
    SELECT 
        CAST(o.created_at AT TIME ZONE 'Europe/Moscow' AS date) AS action_date,
        ofb.fabric_id,
        f.name AS fabric_name,
        ofb.length_m,
        (ofb.pre_cut_id IS NOT NULL) AS is_pre_cut,
        -- Расчет цены в копейках с учетом всех скидок
        ROUND(
          (CASE WHEN pc.id IS NULL THEN (f.price_per_meter * ofb.length_m) ELSE pc.price_rub END) * 100 *
          (1 - CASE 
                 WHEN msp.id IS NOT NULL AND 
                      COALESCE(f.lifecycle, fpc.lifecycle) IN ('clearance', 'on_sale') AND
                      COALESCE(f.is_extra_discount_eligible, fpc.is_extra_discount_eligible) IS TRUE
                 THEN LEAST(COALESCE(f.discount, fpc.discount, 0) + msp.extra_discount, 0.90)
                 ELSE COALESCE(f.discount, fpc.discount, 0)
               END)
        ) AS line_price_kopecks,
        TRUE as is_finalized_order

    FROM orders AS o
    INNER JOIN order_fabric_bindings AS ofb ON o.id = ofb.order_id
    LEFT JOIN fabrics AS f    ON ofb.fabric_id = f.id
    LEFT JOIN pre_cuts AS pc  ON ofb.pre_cut_id = pc.id
    LEFT JOIN fabrics AS fpc  ON pc.fabric_id = fpc.id
    LEFT JOIN monthly_special_promos AS msp 
      ON msp.lucky_day = (o.created_at AT TIME ZONE 'Europe/Moscow')::date AND msp.is_enabled = TRUE
    WHERE o.status NOT IN ('registered', 'cancelled')
      AND o.is_removed_from_delivery_provider = FALSE

    UNION ALL

    -- --- ПОТОК 2: Товары на Виртуальной Полке (уже выкупленные) ---
    SELECT
        CAST(si.added_at AT TIME ZONE 'Europe/Moscow' AS date) AS action_date,
        COALESCE(f.id, fpc.id) AS fabric_id,
        COALESCE(f.name, fpc.name) AS fabric_name,
        si.length_m,
        (si.pre_cut_id IS NOT NULL) AS is_pre_cut,
        -- Аналогичный расчет цены
        ROUND(
          (CASE WHEN si.pre_cut_id IS NULL THEN f.price_per_meter * si.length_m ELSE pc.price_rub END) * 100 *
          (1 - CASE 
                 WHEN msp.id IS NOT NULL AND 
                      COALESCE(f.lifecycle, fpc.lifecycle) IN ('clearance', 'on_sale') AND 
                      COALESCE(f.is_extra_discount_eligible, fpc.is_extra_discount_eligible) IS TRUE
                 THEN LEAST(COALESCE(f.discount, fpc.discount, 0) + msp.extra_discount, 0.90)
                 ELSE COALESCE(f.discount, fpc.discount, 0)
               END)
        ) AS line_price_kopecks,
        FALSE as is_finalized_order

    FROM shelf_items AS si
    LEFT JOIN fabrics AS f   ON si.fabric_id = f.id
    LEFT JOIN pre_cuts AS pc ON si.pre_cut_id = pc.id
    LEFT JOIN fabrics AS fpc ON pc.fabric_id = fpc.id
    LEFT JOIN monthly_special_promos AS msp 
      ON msp.lucky_day = (si.added_at AT TIME ZONE 'Europe/Moscow')::date AND msp.is_enabled = TRUE
    WHERE si.status = 'ON_SHELF'
)
SELECT
    action_date AS sale_date,
    
    -- Общий доход за день (Заказы + Полка)
    SUM(line_price_kopecks) / 100.0 AS total_revenue,
    
    -- Сколько товаров сегодня «ушло» (было продано или положено на полку)
    COUNT(*) AS total_items_handled,
    
    -- Аналитика по типам
    COUNT(*) FILTER (WHERE is_pre_cut) AS pre_cuts_sold,
    COUNT(*) FILTER (WHERE NOT is_pre_cut) AS rolls_handled,
    COALESCE(SUM(length_m) FILTER (WHERE NOT is_pre_cut), 0) AS total_meters,
    
    -- Бренды дня
    jsonb_agg(DISTINCT fabric_name) AS unique_fabrics_names
    
FROM all_revenue_streams
GROUP BY action_date
ORDER BY action_date DESC;

-- Уникальный индекс для REFRESH CONCURRENTLY
CREATE UNIQUE INDEX idx_daily_stats_action_date ON daily_sales_stats (sale_date);

COMMIT;
