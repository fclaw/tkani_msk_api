-- Deploy tkani-api:00010.add_column to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE fabrics ADD COLUMN width INT NOT NULL DEFAULT 0;
 
COMMIT;
