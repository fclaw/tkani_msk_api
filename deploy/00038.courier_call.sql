-- Deploy tkani-api:00038.courier_call to pg

BEGIN;


CREATE OR REPLACE FUNCTION notify_price_calculator()
RETURNS TRIGGER AS $$
BEGIN
    /*
        This trigger fires a 'price_calculation_jobs' notification for three distinct events:
        1. A MANUAL order is INSERTED with a weight.
        2. A BOT order's weight is UPDATED from NULL to a value.
        3. A BOT order's status is UPDATED to 'paid'.
    */

    -- === EVENT 1: Manual Order (INSERT with weight) ===
    IF (TG_OP = 'INSERT' AND NEW.is_bot = FALSE AND NEW.actual_weight_grams IS NOT NULL) THEN
        PERFORM pg_notify(
            'price_calculation_jobs',
            jsonb_build_object('order_id', NEW.id, 'is_bot', NEW.is_bot)::text
        );
        -- RAISE NOTICE 'Fired for Manual Insert: %', NEW.id; -- For debugging
    END IF;
    

    -- === EVENT 2: Bot Order (Weight is set later) ===
    -- Fires when a warehouse worker sets the weight for a 'paid' bot order.
    IF (TG_OP = 'UPDATE' AND NEW.is_bot = TRUE AND NEW.status = 'paid' AND
        NEW.actual_weight_grams IS NOT NULL AND OLD.actual_weight_grams IS NULL) THEN
        
        PERFORM pg_notify(
            'price_calculation_jobs',
            jsonb_build_object('order_id', NEW.id, 'is_bot', NEW.is_bot)::text
        );
        -- RAISE NOTICE 'Fired for Bot Weight Update: %', NEW.id; -- For debugging
    END IF;
    

    -- === EVENT 3: Bot Order (Status changes to 'paid') ===
    -- Fires immediately after payment is confirmed.
    -- Assumes weight can be calculated from cart items, or is set later.
    IF (TG_OP = 'UPDATE' AND NEW.is_bot = TRUE AND 
        NEW.status = 'paid' AND OLD.status <> 'paid') THEN
        
        PERFORM pg_notify(
            'price_calculation_jobs',
            jsonb_build_object('order_id', NEW.id, 'is_bot', NEW.is_bot)::text
        );
        -- RAISE NOTICE 'Fired for Bot Status Paid: %', NEW.id; -- For debugging
    END IF;
    

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Make sure the trigger is attached to BOTH INSERT and UPDATE
DROP TRIGGER IF EXISTS orders_weight_set_trigger ON orders;

CREATE TRIGGER orders_weight_set_trigger
AFTER INSERT OR UPDATE ON orders
FOR EACH ROW
EXECUTE FUNCTION notify_price_calculator();



-- XXX Add DDLs here.
-- Assuming 'dostavista' is not in the ENUM yet

CREATE TYPE pickup_provider AS ENUM ('dostavista');

-- Your courier_pickups table
CREATE TABLE external_courier_pickups (
    id SERIAL PRIMARY KEY,
    provider pickup_provider NOT NULL,
    pickup_date DATE NOT NULL,
    order_id BIGINT NOT NULL,
    cost NUMERIC(10, 2) NOT NULL,
    status TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT unique_external_courier_pickups_pickup_date UNIQUE (pickup_date)
);

CREATE INDEX idx_external_courier_pickups_order_id ON external_courier_pickups(order_id);

ALTER TABLE orders ADD COLUMN courier_pickup_id INT REFERENCES external_courier_pickups(id);

COMMIT;