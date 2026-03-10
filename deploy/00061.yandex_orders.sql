-- Deploy tkani-api:00061.yandex_orders to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE yandex_orders ALTER COLUMN order_id DROP NOT NULL;
ALTER TABLE yandex_orders ADD column label BYTEA;

COMMIT;
