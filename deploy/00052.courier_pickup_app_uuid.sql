-- Deploy tkani-api:00052.courier_pickup_app_uuid to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE courier_pickups ADD COLUMN app_uuid UUID NOT NULL;
ALTER TABLE courier_pickups ADD COLUMN app_status TEXT NOT NULL;

COMMIT;
