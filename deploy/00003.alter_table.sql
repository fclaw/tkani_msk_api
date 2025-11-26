-- Deploy tkani-api:00003.alter_table to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE fabrics ADD COLUMN name TEXT NOT NULL DEFAULT 'Unnamed Fabric';
ALTER TABLE fabrics ADD COLUMN media_group_id TEXT NULL;
ALTER TABLE fabrics ADD COLUMN image_url TEXT;

COMMIT;
