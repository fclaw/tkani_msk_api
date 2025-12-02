-- Revert tkani-api:00007.add_function from pg

BEGIN;

-- XXX Add DDLs here.
DROP FUNCTION IF EXISTS search_fabrics_paginated;

COMMIT;
