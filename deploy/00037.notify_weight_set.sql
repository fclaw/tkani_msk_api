-- Deploy tkani-api:00037.notify_weight_set to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_weight_set()
RETURNS TRIGGER AS $$
BEGIN
    -- This trigger handles the case where an order is already paid,
    -- and the receipt_ready flag is the final piece of information to arrive.
    IF (TG_OP = 'UPDATE' AND
        NEW.status = 'paid' AND
        OLD.status = 'paid' AND -- The status has NOT changed
        NEW.receipt_ready = TRUE AND
        OLD.receipt_ready IS DISTINCT FROM TRUE AND -- Crucially, the flag just CHANGED to TRUE
        NEW.actual_weight_grams IS NOT NULL)
    THEN
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
