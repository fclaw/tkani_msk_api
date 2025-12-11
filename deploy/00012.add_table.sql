-- Deploy tkani-api:00012.add_table to pg

BEGIN;

-- XXX Add DDLs here.

-- ========================================================
-- PRE-REQUISITES: ENUM Type
-- ========================================================
-- (This can be in a separate, earlier migration)
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'cart_item_type') THEN
        CREATE TYPE cart_item_type AS ENUM ('roll', 'pre_cut');
    END IF;
END$$;

-- ========================================================
-- TABLE 1: carts
-- ========================================================
-- This table represents the cart "header" and is owned by a user.

CREATE TABLE carts (
    id BIGSERIAL PRIMARY KEY,
    
    -- Each user can only have one active cart at a time.
    telegram_user_id BIGINT NOT NULL UNIQUE,

    -- is_active is now redundant. An existing row for a user IS the active cart.
    -- We will manage lifecycle with timestamps.
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Index for quickly finding a user's cart
CREATE INDEX idx_carts_user_id ON carts(telegram_user_id);
COMMENT ON TABLE carts IS 'Represents an active shopping cart owned by a Telegram user.';

-- ========================================================
-- TABLE 2: cart_items
-- ========================================================
-- This table stores the individual items within a cart.

CREATE TABLE cart_items (
    id BIGSERIAL PRIMARY KEY,

    -- Foreign key to the parent cart. ON DELETE CASCADE cleans up items automatically.
    cart_id BIGINT NOT NULL REFERENCES carts(id) ON DELETE CASCADE,
    
    item_type cart_item_type NOT NULL,
    
    -- Foreign keys to the actual products
    fabric_id BIGINT NULL REFERENCES fabrics(id) ON DELETE RESTRICT,
    pre_cut_id BIGINT NULL REFERENCES pre_cuts(id) ON DELETE RESTRICT,
    
    -- The requested length for a 'roll' cut.
    length_m DECIMAL(10, 2) NULL,
    
    -- Timestamps
    added_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    
    telegram_url TEXT NOT NULL,

    -- === Business Rules ===
    
    -- Ensure length is only set for rolls
    CONSTRAINT item_type_length_check CHECK (
        (item_type = 'roll' AND length_m IS NOT NULL) OR
        (item_type = 'pre_cut' AND length_m IS NULL)
    ),
    
    -- Ensure pre_cut_id is only set for pre-cuts
    CONSTRAINT item_type_pre_cut_check CHECK (
        (item_type = 'pre_cut' AND pre_cut_id IS NOT NULL) OR
        (item_type = 'roll' AND pre_cut_id IS NULL)
    ),
    
    -- Prevent adding the same pre-cut to the same cart twice
    CONSTRAINT unique_cart_pre_cut UNIQUE (cart_id, pre_cut_id)
);

CREATE INDEX idx_cart_items_cart_id ON cart_items(cart_id);
COMMENT ON TABLE cart_items IS 'Stores the individual items belonging to a shopping cart.';


-- ========================================================
-- ADVANCED LOGIC: Max 5 Items Per Cart
-- ========================================================
-- We enforce this with a trigger function.

CREATE OR REPLACE FUNCTION check_cart_item_limit()
RETURNS TRIGGER AS $$
DECLARE
    item_count INT;
BEGIN
    -- Lock the parent cart row to prevent race conditions
    PERFORM 1 FROM carts WHERE id = NEW.cart_id FOR UPDATE;

    -- Count existing items in the cart
    SELECT count(*) INTO item_count FROM cart_items WHERE cart_id = NEW.cart_id;
    
    -- Check if the limit has been reached
    IF item_count >= 5 THEN
        RAISE EXCEPTION 'Cart item limit reached. A maximum of 5 items is allowed.';
    END IF;
    
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER enforce_cart_limit
BEFORE INSERT ON cart_items
FOR EACH ROW
EXECUTE PROCEDURE check_cart_item_limit();

-- (Your set_updated_at_timestamp triggers would also go here for both tables)

ALTER TABLE orders DROP COLUMN IF EXISTS fabric_id;
ALTER TABLE orders DROP COLUMN IF EXISTS length_m;
ALTER TABLE orders DROP COLUMN IF EXISTS pre_cut_id;
ALTER TABLE orders DROP COLUMN IF EXISTS telegram_url;
ALTER TABLE orders DROP CONSTRAINT IF EXISTS order_type_check;

COMMIT;
