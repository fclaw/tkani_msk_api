-- Deploy tkani-api:00030.order_event to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_price_calculator()
RETURNS TRIGGER AS $$
BEGIN
     -- === CASE 1: A NEW order is created and ALREADY has a weight ===
    IF TG_OP = 'INSERT' AND NEW.actual_weight_grams IS NOT NULL AND NEW.is_bot = FALSE THEN
        
        -- Send a notification on the 'price_calculation_jobs' channel.
        -- The payload is a simple JSON with the order_id.
        PERFORM pg_notify(
            'price_calculation_jobs',
            jsonb_build_object(
              'order_id', NEW.id, 
              'is_bot', NEW.is_bot)::text
        );
        
    END IF;
    
    -- === CASE 2: An EXISTING order is updated with a weight ===
    -- The trigger fires when actual_weight_grams transitions from NULL to a value.
    IF TG_OP = 'UPDATE' AND NEW.actual_weight_grams IS NOT NULL AND NEW.status = 'paid' AND NEW.is_bot = TRUE THEN
        PERFORM pg_notify(
            'price_calculation_jobs',
            jsonb_build_object(
              'order_id', NEW.id, 
              'is_bot', NEW.is_bot)::text
        );
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;


COMMIT;
