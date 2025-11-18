-- Revert tkani-api:3.alter_table from pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE fabrics DROP COLUMN article;

COMMIT;
