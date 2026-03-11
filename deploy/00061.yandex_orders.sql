-- Deploy tkani-api:00061.yandex_orders to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE yandex_orders ALTER COLUMN order_id DROP NOT NULL;
ALTER TABLE yandex_orders ADD column label BYTEA;
ALTER TABLE yandex_orders 
ADD COLUMN status_history JSONB NOT NULL DEFAULT '[]'::jsonb;

-- Optional: Add an index if you plan to search inside history
CREATE INDEX idx_yandex_status_history ON yandex_orders USING gin (status_history);

COMMIT;
