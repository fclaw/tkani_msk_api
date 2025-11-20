-- Revert tkani-api:8.add_table from pg

BEGIN;

-- XXX Add DDLs here.
DROP TABLE IF EXISTS order_telegram_bindings;

COMMIT;
