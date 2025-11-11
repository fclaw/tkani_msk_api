-- Deploy tkani-api:1.create_tables to pg

BEGIN;

-- XXX Add DDLs here.

CREATE TABLE fabrics (
  id SERIAL PRIMARY KEY,
  -- ... all other columns from our schema.sql
  description TEXT NOT NULL,
  price_per_meter INTEGER NOT NULL,
  total_length_m DECIMAL(5, 2) NOT NULL DEFAULT 0.00,
  available_length_m DECIMAL(5, 2) NOT NULL DEFAULT 0.00,
  in_stock BOOLEAN NOT NULL DEFAULT TRUE, -- whether the fabric is in stock
  selling_in_pre_cuts BOOLEAN NOT NULL DEFAULT FALSE -- whether the fabric is being sold in pre-cuts
);

CREATE TABLE pre_cuts (
  id SERIAL PRIMARY KEY,
  fabric_id INTEGER NOT NULL REFERENCES fabrics(id) ON DELETE CASCADE,
  length_m DECIMAL(4, 2) NOT NULL,
  price_rub INTEGER NOT NULL,
  in_stock BOOLEAN NOT NULL DEFAULT TRUE
);

COMMIT;
