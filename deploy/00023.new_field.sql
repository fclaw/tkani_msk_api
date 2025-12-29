-- Deploy tkani-api:00023.new_field to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders ADD COLUMN tariff INT NOT NULL DEFAULT 136;

COMMIT;
