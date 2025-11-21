-- Deploy tkani-api:9.add_table to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TABLE order_fabric_bindings (
  id SERIAL PRIMARY KEY,
  fabric_id INTEGER NOT NULL REFERENCES fabrics(id) ON DELETE CASCADE,
  order_id TEXT NOT NULL REFERENCES orders(id) ON DELETE CASCADE,
  length_m DECIMAL(4, 2) NULL,
  pre_cut INTEGER NULL REFERENCES pre_cuts(id) ON DELETE CASCADE
);

ALTER TABLE fabrics ADD COLUMN pre_cut_required BOOL NOT NULL DEFAULT FALSE;

COMMIT;
