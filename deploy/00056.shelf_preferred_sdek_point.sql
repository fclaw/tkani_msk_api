-- Deploy tkani-api:00056.shelf_preferred_sdek_point to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE shelves ADD COLUMN preferred_sdek_point TEXT

COMMIT;
