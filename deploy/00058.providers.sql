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
  receipt_ready BOOLEAN NOT NULL DEFAULT FALSE,
  receipt_uuid UUID,
  keep_free_until TIMESTAMPTZ
);

COMMIT;
