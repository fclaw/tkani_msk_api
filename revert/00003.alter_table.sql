-- Revert tkani-api:00003.alter_table from pg

BEGIN;

-- XXX Add DDLs here.
DROP COLUMN IF EXISTS name FROM fabrics;
DROP COLUMN IF EXISTS media_group_id FROM fabrics;
DROP COLUMN IF EXISTS image_url FROM fabrics;

COMMIT;
