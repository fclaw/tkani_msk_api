-- Deploy tkani-api:00037.notify_weight_set to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_weight_set()
RETURNS TRIGGER AS $$
BEGIN
    -- This single block covers both bot and non-bot orders.
    -- It fires only on the specific state transition we care about.
    IF (TG_OP = 'UPDATE' AND
        NEW.status = 'paid' AND
        OLD.status <> 'paid' AND -- Ensures this fires only once when the status changes to 'paid'
        NEW.actual_weight_grams IS NOT NULL AND
        NEW.receipt_ready = TRUE) -- The new condition
    THEN
        -- Send a notification on the 'order_weighed_events' channel.
        -- The payload is a simple JSON with the order_id and its weight.
        PERFORM pg_notify(
            'order_weighed_events',
            jsonb_build_object(
                'order_id', NEW.id,
                'weight_grams', NEW.actual_weight_grams
            )::text
        );
    END IF;

    -- Always return the new record in an UPDATE trigger
    RETURN NEW;

END;
$$ LANGUAGE plpgsql;

-- Make sure it's attached correctly
CREATE TRIGGER orders_weighed_trigger
AFTER INSERT OR UPDATE ON orders
FOR EACH ROW
EXECUTE FUNCTION notify_weight_set();

COMMIT;
