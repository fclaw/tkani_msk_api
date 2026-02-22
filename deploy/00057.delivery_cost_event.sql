-- Deploy tkani-api:00057.delivery_cost_event to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE orders ADD COLUMN delivery_cost INT;

CREATE OR REPLACE FUNCTION notify_delivery_cost_set()
RETURNS TRIGGER AS $$
DECLARE
    -- Variable to hold the Telegram binding info
    binding_record RECORD;
BEGIN
    -- 1. Only act if the delivery_cost was updated and is now set (NOT NULL)
    IF (OLD.delivery_cost IS DISTINCT FROM NEW.delivery_cost) AND NEW.delivery_cost IS NOT NULL THEN

        -- 2. Look up the Telegram IDs linked to this order
        SELECT chat_id, message_id 
        INTO binding_record
        FROM order_telegram_bindings
        WHERE order_id = NEW.id OR shelf_order_id = NEW.id
        LIMIT 1;

        -- 3. Only notify if we found an existing Telegram chat to reply to
        IF FOUND THEN
            PERFORM pg_notify(
                'delivery_cost_jobs', -- Channel name
                jsonb_build_object(
                    'order_id', NEW.order_id,      -- The readable order ID string
                    'delivery_cost', NEW.delivery_cost,
                    'chat_id', binding_record.chat_id,
                    'message_id', binding_record.message_id
                )::text
            );
        END IF;

    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER delivery_cost_set_trigger
AFTER UPDATE ON orders
FOR EACH ROW
EXECUTE FUNCTION notify_delivery_cost_set();

COMMIT;
