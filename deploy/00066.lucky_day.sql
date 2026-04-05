-- Deploy tkani-api:00066.lucky_day.sql to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TABLE monthly_special_promos (
    id SERIAL PRIMARY KEY,
    
    -- The target day of the month for the promotion (1-31). If the month has fewer days, it will apply to the last day.
    lucky_day DATE NOT NULL,
    
    -- Extra discount (e.g., 0.10 for 10%)
    extra_discount NUMERIC(3, 2) NOT NULL,
    
    -- Master switch for the whole feature
    is_enabled BOOLEAN NOT NULL DEFAULT FALSE,
        
    message_id BIGINT, -- Optional: Link to a Telegram message announcing the promo 

    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE OR REPLACE FUNCTION get_current_lucky_discount()
RETURNS NUMERIC AS $$
DECLARE
    v_lucky_discount NUMERIC;
BEGIN
    SELECT extra_discount 
    INTO v_lucky_discount
    FROM monthly_special_promos
    WHERE is_enabled = TRUE 
    AND lucky_day = (now() AT TIME ZONE 'Europe/Moscow')::date;
    RETURN COALESCE(v_lucky_discount, 0);
END;
$$ LANGUAGE plpgsql STABLE;

CREATE OR REPLACE FUNCTION calculate_total_discount(
    regular_discount FLOAT8, -- Changed from NUMERIC to FLOAT8
    lifecycle_status TEXT
) RETURNS FLOAT8 AS $$    -- Changed return to FLOAT8
DECLARE
    v_lucky_off FLOAT8 := 0;
BEGIN
    IF lifecycle_status IN ('clearance', 'on_sale') THEN
        v_lucky_off := get_current_lucky_discount(); -- Ensure this returns numeric or float8
    END IF;

    -- Using LEAST with float8
    RETURN LEAST(COALESCE(regular_discount, 0)::float8 + v_lucky_off::float8, 0.90::float8);
END;
$$ LANGUAGE plpgsql STABLE;


COMMIT;
