-- Deploy tkani-api:00058.providers to pg

BEGIN;

-- XXX Add DDLs here.
-- sdek: id, delivery_point, tracking_number, order_uuid, status, tariff
-- yandex: id, delivery_point, order_id, status, tariff
CREATE TABLE sdek_orders (
  id SERIAL PRIMARY KEY,
  delivery_point TEXT NOT NULL,
  tracking_number TEXT NOT NULL,
  order_uuid UUID NOT NULL,
  status TEXT,
  tariff INTEGER NOT NULL,
  receipt_uuid UUID,
  keep_free_until TIMESTAMPTZ,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  -- Temporary column to link back to orders during migration
  temp_original_order_id INTEGER 
);

CREATE TABLE yandex_orders (
  id SERIAL PRIMARY KEY,
  delivery_point TEXT NOT NULL,
  order_id TEXT NOT NULL,
  status TEXT,
  tariff TEXT NOT NULL
);

ALTER TABLE orders ADD COLUMN sdek_order_id INTEGER REFERENCES sdek_orders(id);
ALTER TABLE orders ADD COLUMN yandex_order_id INTEGER REFERENCES yandex_orders(id);

-- drop not null constraints on delivery_provider_id and delivery_point_id since they are now optional
ALTER TABLE orders ALTER COLUMN delivery_provider_id DROP NOT NULL;
ALTER TABLE orders ALTER COLUMN delivery_point_id DROP NOT NULL;

-- 3. Data Migration:
-- We only migrate rows that have SDEK data (using sdek_request_uuid as the indicator)
INSERT INTO sdek_orders (
  delivery_point, 
  tracking_number, 
  order_uuid, 
  tariff, 
  receipt_uuid, 
  temp_original_order_id
)
SELECT 
  delivery_point_id, 
  sdek_tracking_number, 
  sdek_request_uuid, 
  tariff, 
  receipt_uuid, 
  id -- Storing the old order.id here for the next step
FROM orders
WHERE sdek_request_uuid IS NOT NULL;

-- 4. Update the 'orders' table to link to the new records
UPDATE orders
SET sdek_order_id = sdek_orders.id
FROM sdek_orders
WHERE orders.id = sdek_orders.temp_original_order_id;

-- 5. Cleanup: 
-- Remove the temporary helper column
ALTER TABLE sdek_orders DROP COLUMN temp_original_order_id;

-- 6. Cleanup:
-- Remove the original columns from the orders table
ALTER TABLE orders DROP COLUMN delivery_provider_id;
ALTER TABLE orders DROP COLUMN delivery_point_id;
ALTER TABLE orders DROP COLUMN sdek_tracking_number;
ALTER TABLE orders DROP COLUMN sdek_request_uuid;
ALTER TABLE orders DROP COLUMN tariff;
ALTER TABLE orders DROP COLUMN receipt_uuid;
ALTER TABLE orders DROP COLUMN keep_free_until;

COMMIT;
