-- Deploy tkani-api:00062.yandex_order_prepaid to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE yandex_orders ADD COLUMN is_prepaid BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE yandex_orders ADD COLUMN prepaid_cost INTEGER; -- in kopecks

COMMIT;
