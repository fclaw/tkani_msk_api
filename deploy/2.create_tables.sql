BEGIN;

-- This migration creates the main 'orders' table to store customer purchase information.
-- It includes constraints to enforce business logic, such as ensuring an order is
-- either for a custom cut or a pre-cut piece, but not both.

-- First, we create an ENUM type for the order status. This is more robust
-- and type-safe than using a simple TEXT field.
CREATE TYPE order_status AS ENUM (
    'new', -- Order created, waiting for payment confirmation from Tinkoff.
    'paid',            -- Payment confirmed. Ready for processing.
    'on_route',    -- Order has been shipped to the customer.
    'delivered',     -- Order delivered and finalized.
    'completed',
    'cancelled'
);

-- Now, create the 'orders' table.
CREATE TABLE orders (
    -- The primary key for the order, using the human-friendly generated ID.
    id TEXT PRIMARY KEY,

    -- Foreign key to the 'fabrics' table. Assumes a 'fabrics' table with an 'id' column.
    fabric_id INT NOT NULL REFERENCES fabrics(id),

    -- Purchase Details: exactly one of length_m or pre_cut_id must be non-null.
    length_m DOUBLE PRECISION,
    pre_cut_id INT,

    -- Customer & Delivery Information
    customer_full_name TEXT NOT NULL,
    customer_phone TEXT NOT NULL,
    delivery_provider_id TEXT NOT NULL, -- e.g., 'sdek', 'boxberry'
    delivery_point_id TEXT NOT NULL,

    -- A link back to the Telegram post where the fabric was advertised.
    -- This is great for analytics and context.
    telegram_url TEXT,

    -- Timestamps and Status
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    status order_status NOT NULL DEFAULT 'new',
    internal_notification_message_id INT


    -- This business rule is critical and is enforced at the database level.
    -- It ensures data integrity.
    CONSTRAINT order_type_check CHECK (
        (length_m IS NOT NULL AND pre_cut_id IS NULL) OR
        (length_m IS NULL AND pre_cut_id IS NOT NULL)
    )
);

-- Create a trigger to automatically update the 'updated_at' timestamp
-- whenever a row is changed. This is a standard best practice.
CREATE OR REPLACE FUNCTION set_updated_at_timestamp()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER set_orders_updated_at
BEFORE UPDATE ON orders
FOR EACH ROW
EXECUTE PROCEDURE set_updated_at_timestamp();

-- Add indexes for columns that will be frequently queried.
CREATE INDEX idx_orders_fabric_id ON orders(fabric_id);
CREATE INDEX idx_orders_customer_phone ON orders(customer_phone);
CREATE INDEX idx_orders_status ON orders(status);
CREATE INDEX idx_orders_created_at ON orders(created_at);

-- Add comments to the table and columns for documentation purposes.
-- This makes the schema self-documenting.
COMMENT ON TABLE orders IS 'Stores customer orders placed via the Telegram bot.';
COMMENT ON COLUMN orders.id IS 'Primary key, human-friendly ID like ORD-YYYYMMDD-XXXXXX.';
COMMENT ON COLUMN orders.length_m IS 'Length in meters for a custom fabric cut.';
COMMENT ON COLUMN orders.pre_cut_id IS 'ID for a specific pre-cut fabric piece.';
COMMENT ON COLUMN orders.status IS 'The current status of the order in its lifecycle.';
COMMENT ON COLUMN orders.telegram_url IS 'Link to the Telegram post showing the ordered fabric.';
COMMENT ON COLUMN orders.internal_notification_message_id IS 'The Telegram message_id of the new order notification in the internal channel.';

COMMIT;