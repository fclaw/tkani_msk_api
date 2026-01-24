
BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_cancel_event()
RETURNS TRIGGER AS $$
BEGIN
    -- This condition now ensures the event fires only ONCE when the status
    -- changes TO 'cancelled' from any other state.
    IF (TG_OP = 'UPDATE' AND NEW.status = 'cancelled' AND OLD.status != 'cancelled') THEN
        PERFORM pg_notify(
            'order_cancel_events',
            jsonb_build_object(
                'order_id', NEW.id,
                'sdek_uuid', NEW.sdek_request_uuid
            )::text
        );
    END IF;

    -- For an AFTER trigger, the return value is ignored, but it's required syntax.
    -- Returning NEW is standard practice.
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Drop the old trigger if it exists, to ensure a clean slate
DROP TRIGGER IF EXISTS orders_cancel_trigger ON orders;

-- Create the trigger. We can make it slightly more efficient by only
-- firing it ON UPDATE, since an order cannot be created in a cancelled state.
CREATE TRIGGER orders_cancel_trigger
AFTER UPDATE ON orders
FOR EACH ROW
-- We can also add a 'WHEN' clause to prevent the function from running at all
-- unless the status field actually changes. This is a performance optimization.
WHEN (OLD.status IS DISTINCT FROM NEW.status)
EXECUTE FUNCTION notify_cancel_event();

COMMIT;
