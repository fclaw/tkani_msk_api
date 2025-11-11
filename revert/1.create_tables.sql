-- Revert tkani-api:1.create_tables from pg

BEGIN;

-- XXX Add DDLs here.

DROP TABLE IF EXISTS pre_cuts;
DROP TABLE IF EXISTS fabrics;

COMMIT;
