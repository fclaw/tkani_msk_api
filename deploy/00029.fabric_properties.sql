-- Deploy tkani-api:00029.fabric_properties to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TYPE fabric_density AS ENUM (
   'light',
   'semi_light',
   'dense'
);

ALTER TABLE fabrics ADD column weight_per_metre NUMERIC(16, 2) NOT NULL DEFAULT 0;
ALTER TABLE fabrics ADD column density fabric_density NOT NULL DEFAULT 'light';

COMMIT;
