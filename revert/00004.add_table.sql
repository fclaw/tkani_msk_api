-- Revert tkani-api:00004.add_table from pg

BEGIN;

-- XXX Add DDLs here.
DROP TABLE IF EXISTS payments;
DROP TYPE IF EXISTS payment_provider;
DROP TYPE IF EXISTS payment_status;

COMMIT;
