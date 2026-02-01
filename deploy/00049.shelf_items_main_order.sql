-- Deploy tkani-api:00049.shelf_items_main_order to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE shelf_items ADD COLUMN main_order_id TEXT;

CREATE INDEX idx_shelf_items_main_order_id ON shelf_items(main_order_id);

COMMIT;
