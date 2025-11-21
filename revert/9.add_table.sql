-- Revert tkani-api:9.add_table from pg

BEGIN;

-- XXX Add DDLs here.
DROP TABLE order_fabric_bindings IF EXISTS;

COMMIT;
