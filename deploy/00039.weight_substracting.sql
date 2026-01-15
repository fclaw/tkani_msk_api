-- Deploy tkani-api:00039.weight_substracting to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_order_weight_subtraction()
RETURNS TRIGGER AS $$
BEGIN
    -- Fire only if status changes FROM 'paid' to something else
    IF TG_OP = 'UPDATE' AND OLD.status = 'paid' AND NEW.status <> 'paid' THEN
        PERFORM pg_notify(
            'order_weight_subtraction_events',
            jsonb_build_object(
                'order_id', NEW.id,
                'weight_grams', OLD.actual_weight_grams -- Send the OLD weight
            )::text
        );
    END IF;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER order_weight_subtraction_trigger
AFTER UPDATE ON orders
FOR EACH ROW
WHEN (OLD.status IS DISTINCT FROM NEW.status)
EXECUTE FUNCTION notify_order_weight_subtraction();


COMMIT;
