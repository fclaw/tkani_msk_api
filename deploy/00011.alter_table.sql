-- Deploy tkani-api:00011.alter_table to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders DROP COLUMN IF EXISTS fabric_id;
ALTER TABLE orders DROP COLUMN IF EXISTS length_m;
ALTER TABLE orders DROP COLUMN IF EXISTS pre_cut_id;

COMMIT;
