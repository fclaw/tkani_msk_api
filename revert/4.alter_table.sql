-- Revert tkani-api:4.alter_table from pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders DROP COLUMN sdek_tracking_number;

COMMIT;
