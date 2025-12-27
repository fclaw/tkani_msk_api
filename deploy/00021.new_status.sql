-- Deploy tkani-api:00021.new_status to pg

BEGIN;

-- XXX Add DDLs here.
-- migration_add_pickedup_status.sql
ALTER TYPE order_status ADD VALUE 'picked_up_by_courier';


-- 1. Create an ENUM type for the pickup request status.
--    This mirrors the 'state' values from the SDEK API.
CREATE TYPE pickup_status AS ENUM (
    'accepted',  -- Request received and passed initial validation.
    'waiting',   -- Request is in the queue, awaiting processing.
    'successful',-- A courier has been assigned.
    'invalid',   -- The request failed validation and needs to be corrected.
    'cancelled'  -- The request was cancelled.
);

-- 2. Create the main 'courier_pickups' table.
--    Each row represents one "call courier" request to SDEK.
CREATE TABLE courier_pickups (
    -- The request UUID from SDEK is the perfect Primary Key.
    request_uuid UUID PRIMARY KEY,
    
    -- The current status of this pickup request.
    status pickup_status NOT NULL,
    
    -- Date for which the pickup was requested.
    pickup_date DATE NOT NULL,
    
    -- Optional fields for storing any error details from the SDEK API.
    error_code TEXT,
    error_message TEXT,
    
    -- Timestamps for tracking.
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- 3. Create a link between orders and pickups.
--    We need a way to know which orders belong to which pickup.
--    Add a foreign key column to your 'orders' table.
ALTER TABLE orders
ADD COLUMN courier_pickup_uuid UUID REFERENCES courier_pickups(request_uuid) ON DELETE SET NULL;

-- Add an index for faster lookups.
CREATE INDEX idx_orders_courier_pickup_uuid ON orders(courier_pickup_uuid);


-- 4. Create the trigger to automatically update the 'updated_at' timestamp.
--    (Assuming you have the 'set_updated_at_timestamp' function already).
CREATE TRIGGER set_courier_pickups_updated_at
BEFORE UPDATE ON courier_pickups
FOR EACH ROW EXECUTE PROCEDURE set_updated_at_timestamp();

COMMIT;