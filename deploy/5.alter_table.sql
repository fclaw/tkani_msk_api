-- Deploy tkani-api:5.alter_table to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders
ADD COLUMN order_id TEXT;

-- Create an index on the sdek_tracking_number column for fast lookups.
CREATE INDEX idx_orders_order_id ON orders(order_id);

COMMENT ON COLUMN orders.order_id IS 'Internal identificator in tkani api.';
COMMIT;
