-- Deploy tkani-api:00050.order to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TYPE order_status ADD VALUE 'added_to_pickup_queue';

CREATE OR REPLACE FUNCTION notify_order_weight_subtraction()
RETURNS TRIGGER AS $$
BEGIN
    /*
        This trigger fires an event to subtract weight from the in-memory
        Dostavista batch counter. It should ONLY fire if an order is
        removed from the 'paid' queue for a reason OTHER than being
        assigned to a courier pickup.
    */
    IF TG_OP = 'UPDATE' THEN
        -- THE KEY CONDITIONS:
        IF 
            -- 1. The order was in the 'paid' state (eligible for a batch)
            OLD.status = 'added_to_pickup_queue'
            
            AND

            -- 2. Its status is changing to something else (e.g., 'cancelled')
            NEW.status = 'cancelled'
            
            AND

            -- 3. THE SAFETY CHECK (YOUR FIX):
            --    This order has NOT been assigned to a pickup batch yet.
            NEW.courier_pickup_id IS NULL
            
            AND

            -- 4. Another safety check: ensure it had a weight to subtract.
            OLD.actual_weight_grams IS NOT NULL

        THEN
            -- All conditions met. It's a valid cancellation/removal.
            PERFORM pg_notify(
                'order_weight_subtraction_events',
                jsonb_build_object(
                    'order_id', NEW.id,
                    'weight_grams', OLD.actual_weight_grams -- Send the weight it used to have
                )::text
            );
        END IF;
    END IF;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;


COMMIT;
