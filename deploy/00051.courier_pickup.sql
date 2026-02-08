-- Deploy tkani-api:00051.courier_pickup to pg

BEGIN;

-- XXX Add DDLs here.
DROP TABLE IF EXISTS courier_pickups CASCADE;
CREATE TABLE courier_pickups (
    id SERIAL PRIMARY KEY,
    sdek_uuid UUID NOT NULL,
    pickup_date DATE NOT NULL
);

ALTER TABLE orders DROP COLUMN IF EXISTS courier_pickup_uuid;
ALTER TABLE orders ADD COLUMN IF NOT EXISTS sdek_courier_pickup_id INTEGER REFERENCES courier_pickups(id);

COMMIT;
