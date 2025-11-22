BEGIN;

-- ========================================================
-- 1. PRE-REQUISITES: Types and Functions
-- ========================================================

-- Enum for type-safe order status
CREATE TYPE order_status AS ENUM (
    'registered',  -- Created, waiting for payment
    'paid',        -- Payment confirmed
    'on_route',    -- Shipped
    'delivered',   -- Received
    'completed',   -- Finalized
    'cancelled'    -- Aborted
);

-- Function to auto-update timestamps
CREATE OR REPLACE FUNCTION set_updated_at_timestamp()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- ========================================================
-- 2. TABLES: Core Inventory
-- ========================================================

-- FABRICS
CREATE TABLE fabrics (
    id SERIAL PRIMARY KEY,
    article TEXT NOT NULL, -- SKU, Business ID
    description TEXT NOT NULL,
    price_per_meter INTEGER NOT NULL,
    
    -- Inventory Logic: DECIMAL(10,2) prevents rounding/overflow errors
    total_length_m DECIMAL(10, 2) NOT NULL DEFAULT 0.00,
    available_length_m DECIMAL(10, 2) NOT NULL DEFAULT 0.00,
    
    -- Status Flags
    in_stock BOOLEAN NOT NULL DEFAULT TRUE,
    is_sold BOOLEAN NOT NULL DEFAULT FALSE, -- True if roll is finished
    
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),

    -- Constraints
    CONSTRAINT fabrics_article_unique UNIQUE (article)
);

CREATE TRIGGER set_fabrics_updated_at
BEFORE UPDATE ON fabrics
FOR EACH ROW EXECUTE PROCEDURE set_updated_at_timestamp();

-- PRE-CUTS (Scraps/Pieces)
CREATE TABLE pre_cuts (
    id SERIAL PRIMARY KEY,
    fabric_id INTEGER NOT NULL REFERENCES fabrics(id) ON DELETE CASCADE,
    
    length_m DECIMAL(10, 2) NOT NULL,
    price_rub INTEGER NOT NULL,
    in_stock BOOLEAN NOT NULL DEFAULT TRUE
);

-- ========================================================
-- 3. TABLES: Orders & Sales
-- ========================================================

-- MAIN ORDERS TABLE
CREATE TABLE orders (
    -- ID Format: ORD-YYYYMMDD-XXXXXX
    id TEXT PRIMARY KEY,

    -- Inventory Link
    fabric_id INT NOT NULL REFERENCES fabrics(id),

    -- Purchase Logic: One or the other must be set
    length_m DECIMAL(10, 2),
    pre_cut_id INT REFERENCES pre_cuts(id) ON DELETE SET NULL,

    -- Customer Info
    customer_full_name TEXT NOT NULL,
    customer_phone TEXT NOT NULL,
    
    -- Delivery Info
    delivery_provider_id TEXT NOT NULL,
    delivery_point_id TEXT NOT NULL,
    sdek_tracking_number TEXT,
    sdek_request_uuid UUID,

    -- Context & Notifications
    telegram_url TEXT,
    internal_notification_message_id BIGINT, -- Changed to BIGINT for safety

    -- Lifecycle
    status order_status NOT NULL DEFAULT 'registered',
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),

    -- Integrity Constraint: Buy a Length OR a Pre-cut, not mixed here
    CONSTRAINT order_type_check CHECK (
        (length_m IS NOT NULL AND pre_cut_id IS NULL) OR
        (length_m IS NULL AND pre_cut_id IS NOT NULL)
    )
);

CREATE TRIGGER set_orders_updated_at
BEFORE UPDATE ON orders
FOR EACH ROW EXECUTE PROCEDURE set_updated_at_timestamp();

-- ORDER FABRIC BINDINGS (Detailed Snapshot)
-- Used to preserve exactly what was bought even if the fabric is later deleted.
CREATE TABLE order_fabric_bindings (
    id SERIAL PRIMARY KEY,
    order_id TEXT NOT NULL REFERENCES orders(id) ON DELETE CASCADE,
    fabric_id INTEGER NOT NULL REFERENCES fabrics(id) ON DELETE CASCADE,
    
    length_m DECIMAL(10, 2) NULL,
    pre_cut_id INTEGER NULL REFERENCES pre_cuts(id) ON DELETE SET NULL
);

-- TELEGRAM NOTIFICATION BINDINGS
-- Stores IDs to allow the bot to edit specific messages later (e.g., "Paid")
CREATE TABLE order_telegram_bindings (
    order_id TEXT PRIMARY KEY REFERENCES orders(id) ON DELETE CASCADE,
    chat_id BIGINT NOT NULL,
    message_id BIGINT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- ========================================================
-- 4. INDEXES
-- ========================================================

CREATE INDEX idx_orders_fabric_id ON orders(fabric_id);
CREATE INDEX idx_orders_customer_phone ON orders(customer_phone);
CREATE INDEX idx_orders_status ON orders(status);
CREATE INDEX idx_orders_created_at ON orders(created_at);
CREATE INDEX idx_orders_sdek_tracking_number ON orders(sdek_tracking_number);

-- ========================================================
-- 5. DOCUMENTATION
-- ========================================================

COMMENT ON TABLE orders IS 'Stores customer orders placed via the Telegram bot.';
COMMENT ON COLUMN orders.id IS 'Primary key, human-friendly ID like ORD-YYYYMMDD-XXXXXX.';
COMMENT ON COLUMN orders.length_m IS 'Length in meters for a custom fabric cut.';
COMMENT ON COLUMN orders.pre_cut_id IS 'ID for a specific pre-cut fabric piece.';
COMMENT ON COLUMN orders.sdek_tracking_number IS 'The official SDEK tracking number.';
COMMENT ON COLUMN orders.sdek_request_uuid IS 'The tracking UUID returned by SDEK asynchronous registration.';
COMMENT ON COLUMN orders.internal_notification_message_id IS 'Message ID of the notification sent to the admin channel.';

COMMENT ON TABLE fabrics IS 'Main inventory table for Rolls.';
COMMENT ON COLUMN fabrics.article IS 'Unique SKU / Article number.';
COMMENT ON COLUMN fabrics.is_sold IS 'True when available length is effectively zero.';

COMMIT;