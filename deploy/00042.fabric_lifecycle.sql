-- Deploy tkani-api:00042.fabric_lifecycle to pg

BEGIN;

-- XXX Add DDLs here.
-- Enum for type-safe order status
CREATE TYPE fabric_lifecycle AS ENUM (
   'new_arrival',
   'advertised',
   'regular',
   'on_sale',
   'clearance',
   'achieved'
);

ALTER TABLE fabrics ADD COLUMN lifecycle fabric_lifecycle NOT NULL DEFAULT 'new_arrival';
ALTER TABLE fabrics ADD COLUMN lifecycle_changed_at TIMESTAMPTZ NOT NULL DEFAULT NOW();
ALTER TABLE fabrics ADD COLUMN selling_price INTEGER NOT NULL DEFAULT 0;

COMMIT;
