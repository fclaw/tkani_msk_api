-- Deploy tkani-api:00005.alter_table to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE fabrics
ADD COLUMN thumbnail_url TEXT;

COMMIT;
