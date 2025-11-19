-- Revert tkani-api:6.alter_table from pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders DROP COLUMN sdek_request_uuid;

COMMIT;
