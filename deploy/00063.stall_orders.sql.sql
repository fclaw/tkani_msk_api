-- Deploy tkani-api:00063.stall_orders.sql to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TABLE stall_orders_log (
    -- The primary key ensures you never log the same order twice
    order_id TEXT PRIMARY KEY REFERENCES orders(id) ON DELETE CASCADE,
    notified_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Indexing isn't strictly required for a PK, but good for joins
CREATE INDEX idx_stall_orders_log_order_id ON stall_orders_log(order_id);

-- 2. Backfill existing stale orders to prevent immediate bot spam
INSERT INTO stall_orders_log (order_id)
SELECT o.id :: text
FROM orders AS o
INNER JOIN order_telegram_bindings AS ofb ON o.id = ofb.order_id
WHERE o.status = 'paid' 
  AND o.updated_at < (NOW() - INTERVAL '2 days')
ON CONFLICT (order_id) DO NOTHING;

COMMIT;
