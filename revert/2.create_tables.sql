-- Revert tkani-api:2.create_tables from pg

BEGIN;

-- XXX Add DDLs here.
DROP TABLE IF EXISTS orders;

COMMIT;
