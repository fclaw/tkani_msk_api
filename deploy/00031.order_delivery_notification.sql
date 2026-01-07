-- Deploy tkani-api:00031.order_delivery_notification to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TABLE order_delivery_posts (
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  message_id INT NOT NULL DEFAULT 0
);

COMMIT;
