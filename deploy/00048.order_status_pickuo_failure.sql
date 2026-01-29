-- Deploy tkani-api:00048.order_status_pickuo_failure to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TYPE order_status ADD VALUE 'pickup_failed';

COMMIT;
