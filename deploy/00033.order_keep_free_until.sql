-- Deploy tkani-api:00033.order_keep_free_until to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders ADD COLUMN keep_free_until TIMESTAMPTZ;

COMMIT;
