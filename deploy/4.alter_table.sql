-- Deploy tkani-api:4.alter_table to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders
ADD COLUMN sdek_tracking_number TEXT;

-- Create an index on the sdek_tracking_number column for fast lookups.
CREATE INDEX idx_orders_sdek_tracking_number ON orders(sdek_tracking_number);

COMMENT ON COLUMN orders.sdek_tracking_number IS 'The official, permanent SDEK tracking number for the shipment.';

COMMIT;
