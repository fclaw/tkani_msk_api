-- Deploy tkani-api:00037.notify_weight_set to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_weight_set()
RETURNS TRIGGER AS $$
BEGIN
     -- === CASE 1: A NEW order is created and ALREADY has a weight ===
    IF TG_OP = 'INSERT' AND NEW.actual_weight_grams IS NOT NULL 
       AND NEW.is_bot = FALSE THEN
        
        -- Send a notification on the 'price_calculation_jobs' channel.
        -- The payload is a simple JSON with the order_id.
        PERFORM pg_notify(
            'order_weighed_events', -- Use a descriptive channel name
            jsonb_build_object(
                'order_id', NEW.id,
                'weight_grams', NEW.actual_weight_grams
            )::text
        );
        
    END IF;
    
    -- === EVENT 2: A BOT order has its status UPDATED to 'paid' ===
    -- TG_OP is 'UPDATE', is_bot is TRUE, and the status has just changed to 'paid'.
    -- The weight has also been pre-calculated and is available in the NEW record.
    IF (TG_OP = 'UPDATE' AND NEW.is_bot = TRUE AND 
        NEW.status = 'paid' AND OLD.status <> 'paid' AND
        NEW.actual_weight_grams IS NOT NULL) THEN

        PERFORM pg_notify(
            'order_weighed_events',
            jsonb_build_object(
                'order_id', NEW.id,
                'weight_grams', NEW.actual_weight_grams
            )::text
        );
    END IF;

    RETURN NEW;

END;
$$ LANGUAGE plpgsql;

-- Make sure it's attached correctly
CREATE TRIGGER orders_weighed_trigger
AFTER INSERT OR UPDATE ON orders
FOR EACH ROW
EXECUTE FUNCTION notify_weight_set();

COMMIT;
