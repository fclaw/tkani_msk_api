-- Deploy tkani-api:6.alter_table to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders
ADD COLUMN sdek_request_uuid UUID;

COMMENT ON COLUMN orders.sdek_request_uuid IS 'The tracking UUID returned by SDEK asynchronous registration.';
COMMIT;
