-- Deploy tkani-api:00024.order_event to pg
-- migration_add_weight_and_price_trigger.sql

BEGIN;

-- XXX Add DDLs here.

-- Add columns to store the actual weight and the calculated shipping price.
ALTER TABLE orders 
ADD COLUMN actual_weight_grams INT; -- Store weight in grams for precision

ALTER TABLE orders
ADD COLUMN shipping_cost_kopecks INT; -- Store price in kopecks

ALTER TABLE orders
ADD column is_bot BOOLEAN;

ALTER TABLE orders
ADD column length INT;

ALTER TABLE orders
ADD column width INT;

ALTER TABLE orders
ADD column height INT;

ALTER TABLE orders
ADD COLUMN receipt_ready BOOLEAN NOT NULL DEFAULT FALSE;

ALTER TABLE orders
ADD column receipt_uuid UUID;

-- Optional but recommended: Add an index for the worker that will look for these jobs.
CREATE INDEX idx_orders_receipt_ready ON orders (receipt_ready)
WHERE receipt_ready = TRUE;

-- Create the trigger function.
CREATE OR REPLACE FUNCTION notify_price_calculator()
RETURNS TRIGGER AS $$
BEGIN
     -- === CASE 1: A NEW order is created and ALREADY has a weight ===
    IF TG_OP = 'INSERT' AND NEW.actual_weight_grams IS NOT NULL THEN
        
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
    IF TG_OP = 'UPDATE' AND NEW.actual_weight_grams IS NOT NULL AND OLD.actual_weight_grams IS NULL THEN
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


-- Now, create the new one
CREATE TRIGGER orders_weight_set_trigger
-- It MUST listen for both INSERT and UPDATE
AFTER INSERT OR UPDATE ON orders
FOR EACH ROW
EXECUTE FUNCTION notify_price_calculator();

CREATE TABLE manual_order_items (
    id SERIAL PRIMARY KEY,
    
    -- Foreign key to the order it belongs to
    order_id TEXT NOT NULL REFERENCES orders(id) ON DELETE CASCADE,
    
    -- All the details from the YAML, denormalized
    item_name TEXT NOT NULL,
    fabric_type TEXT NOT NULL, -- "roll" or "pre_cut"
    
    -- We can use your 'order_item_type' ENUM here if you have one
    -- fabric_type order_item_type NOT NULL,
    
    price_per_metre NUMERIC(10, 2), -- Nullable, for rolls
    total_price NUMERIC(10, 2) NOT NULL,
    length_m NUMERIC(10, 2), -- Nullable

    weight INT NOT NULL DEFAULT 0,

    article TEXT NOT NULL, -- SKU, Business ID
    
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Add an index for faster lookups
CREATE INDEX idx_manual_order_items_order_id ON manual_order_items(order_id);

CREATE OR REPLACE FUNCTION notify_receipt_generator()
RETURNS TRIGGER AS $$
BEGIN
    -- This trigger should only fire on UPDATE operations.
    -- The condition checks for the state transition: FALSE -> TRUE.
    IF TG_OP = 'UPDATE' AND NEW.receipt_ready = TRUE AND OLD.receipt_ready = FALSE THEN
        
        -- Send a notification on a dedicated channel, e.g., 'receipt_jobs'.
        -- The payload contains the order_id, which is all the worker needs.
        PERFORM pg_notify(
          'receipt_jobs',
          jsonb_build_object(
            'receipt_uuid', NEW.receipt_uuid, 
            'order_id', NEW.id,
            'customer', NEW.customer_full_name)::text
        );
        
    END IF;
    
    -- ALWAYS return NEW for an UPDATE trigger.
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;


-- Attach the trigger to the 'orders' table.
-- First, drop the old one if it exists to prevent duplicates.
DROP TRIGGER IF EXISTS orders_receipt_ready_trigger ON orders;

CREATE TRIGGER orders_receipt_ready_trigger
-- Fire AFTER a row is updated.
AFTER UPDATE ON orders
FOR EACH ROW
-- Optional: A WHEN clause makes the trigger slightly more efficient,
-- as it won't even call the function unless the flag has changed to TRUE.
WHEN (OLD.receipt_ready IS DISTINCT FROM NEW.receipt_ready AND NEW.receipt_ready = TRUE)
EXECUTE FUNCTION notify_receipt_generator();


COMMIT;
