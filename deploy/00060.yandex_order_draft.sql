-- Deploy tkani-api:00060.yandex_order_draft to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE yandex_orders ADD COLUMN draft_order_request JSONB;

COMMIT;
