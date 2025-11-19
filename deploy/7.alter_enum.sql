-- Deploy tkani-api:7.alter_enum to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TYPE order_status RENAME VALUE 'new' TO 'registered';
ALTER TABLE orders ALTER COLUMN status SET DEFAULT 'registered';

COMMIT;
