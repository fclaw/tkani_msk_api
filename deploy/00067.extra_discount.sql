-- Deploy tkani-api:00067.extra_discount to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE fabrics ADD COLUMN is_extra_discount_eligible BOOLEAN NOT NULL DEFAULT FALSE;


CREATE OR REPLACE FUNCTION calculate_total_discount(
    regular_discount FLOAT8, -- Changed from NUMERIC to FLOAT8
    lifecycle_status TEXT,
    is_extra_discount_eligible BOOLEAN
) RETURNS FLOAT8 AS $$    -- Changed return to FLOAT8
DECLARE
    v_lucky_off FLOAT8 := 0;
BEGIN
    IF lifecycle_status IN ('clearance', 'on_sale') AND 
       is_extra_discount_eligible IS TRUE THEN
        v_lucky_off := get_current_lucky_discount(); -- Ensure this returns numeric or float8
    END IF;

    -- Using LEAST with float8
    RETURN LEAST(COALESCE(regular_discount, 0)::float8 + v_lucky_off::float8, 0.90::float8);
END;
$$ LANGUAGE plpgsql STABLE;

COMMIT;
