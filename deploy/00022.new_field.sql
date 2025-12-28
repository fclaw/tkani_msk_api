-- Deploy tkani-api:00022.new_field to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders ADD COLUMN is_measured BOOLEAN NOT NULL DEFAULT FALSE;

COMMIT;
