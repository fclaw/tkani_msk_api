-- Revert tkani-api:5.alter_table from pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders DROP COLUMN order_id;

COMMIT;
