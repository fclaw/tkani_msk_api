-- Deploy tkani-api:00055.order_is_erased to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders ADD COLUMN is_erased BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE shelf_orders ADD COLUMN is_erased BOOLEAN NOT NULL DEFAULT FALSE;

COMMIT;
