-- Deploy tkani-api:00041.price_calulcator to pg

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
    
    -- === CASE 2: An EXISTING order is updated with dimensions ===
    -- The trigger fires when actual_weight_grams transitions from NULL to a value.
    IF TG_OP = 'UPDATE' 
       AND NEW.status = 'paid' 
       AND NEW.is_bot = TRUE 
       -- Prerequisite: Weight must already be there (per your flow)
       AND NEW.actual_weight_grams IS NOT NULL 
       -- Prerequisite: Dimensions must be fully set
       AND NEW.length IS NOT NULL 
       AND NEW.width  IS NOT NULL 
       AND NEW.height IS NOT NULL
       -- The Trigger: Did the dimensions actually change or get filled in this specific update?
       -- This prevents the event from firing if you update 'customer_email' but dimensions were already set.
       AND (
           OLD.length IS DISTINCT FROM NEW.length OR
           OLD.width  IS DISTINCT FROM NEW.width  OR
           OLD.height IS DISTINCT FROM NEW.height
       )
    THEN
        PERFORM pg_notify(
            'price_calculation_jobs',
            jsonb_build_object(
              'order_id', NEW.id, 
              'is_bot', NEW.is_bot
            )::text
        );
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;


COMMIT;
