-- Deploy tkani-api:00049.put_on_shelf_records to pg

BEGIN;

-- XXX Add DDLs here.

CREATE TYPE payment_flow_types AS ENUM (
    'put_on_shelf',
    'ship_now'
);

ALTER TABLE payments ADD COLUMN payment_flow payment_flow_types NOT NULL DEFAULT 'ship_now';


CREATE TYPE shelf_order_status AS ENUM (
    'registered',
    'paid',
    'cancelled'
);

CREATE TABLE shelf_orders (
    id SERIAL PRIMARY KEY,
    order_id TEXT NOT NULL,
    shelf_id INTEGER NOT NULL REFERENCES shelves(id) ON DELETE CASCADE,
    status shelf_order_status NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE shelf_order_items (
    id SERIAL PRIMARY KEY,
    shelf_order_id INTEGER NOT NULL REFERENCES shelf_orders(id) ON DELETE CASCADE,
    fabric_id INTEGER REFERENCES fabrics(id) ON DELETE SET NULL,
    pre_cut_id INTEGER REFERENCES pre_cuts(id) ON DELETE SET NULL,
    length_m DECIMAL(10, 2)
);


-- ====================================================================
--  Modify 'payments' Table
-- ====================================================================

-- Add the 'shelf_order_id' column if it doesn't already exist.
-- Direct support for IF NOT EXISTS on ADD COLUMN.
ALTER TABLE payments ADD COLUMN IF NOT EXISTS shelf_order_id TEXT;

-- Add the CHECK constraint only if a constraint with this name doesn't exist.
-- Direct support for IF NOT EXISTS on ADD CONSTRAINT.
ALTER TABLE payments ADD CONSTRAINT IF NOT EXISTS payments_has_at_least_one_order_id
CHECK (order_id IS NOT NULL OR shelf_order_id IS NOT NULL);

-- Add the 'payment_flow' column if it doesn't exist.
-- The NOT NULL and DEFAULT are part of the same operation.
ALTER TABLE payments ADD COLUMN IF NOT EXISTS payment_flow payment_flow_types NOT NULL DEFAULT 'ship_now';

-- Create an index on the new column only if it doesn't exist.
-- Direct support for IF NOT EXISTS.
CREATE INDEX IF NOT EXISTS idx_payments_shelf_order_id ON payments(shelf_order_id);

-- Create an index on the old column only if it doesn't exist.
CREATE INDEX IF NOT EXISTS idx_payments_order_id ON payments(order_id);


-- ====================================================================
--  Modify 'order_telegram_bindings' Table
-- ====================================================================

-- Add the 'shelf_order_id' column if it doesn't exist.
ALTER TABLE order_telegram_bindings ADD COLUMN IF NOT EXISTS shelf_order_id TEXT;

-- Drop the primary key constraint using a procedural block for safety.
DO $$
BEGIN
   IF EXISTS (
       SELECT 1 FROM information_schema.table_constraints
       WHERE constraint_name = 'order_telegram_bindings_pkey'
       AND table_name = 'order_telegram_bindings'
   ) THEN
       ALTER TABLE order_telegram_bindings DROP CONSTRAINT order_telegram_bindings_pkey;
   END IF;
END $$;
-- NOTE: If your PK has a different name, you must change it above!

-- Add the CHECK constraint only if it doesn't exist.
ALTER TABLE order_telegram_bindings ADD CONSTRAINT IF NOT EXISTS order_telegram_bindings_has_at_least_one_order_id
CHECK (order_id IS NOT NULL OR shelf_order_id IS NOT NULL);

-- Create index on the new column if it doesn't exist.
CREATE INDEX IF NOT EXISTS idx_order_telegram_bindings_shelf_order_id ON order_telegram_bindings(shelf_order_id);

-- Create index on the old column if it doesn't exist.
CREATE INDEX IF NOT EXISTS idx_order_telegram_bindings_order_id ON order_telegram_bindings(order_id);

ALTER TABLE payments ALTER COLUMN order_id DROP NOT NULL;
ALTER TABLE order_telegram_bindings ALTER COLUMN order_id DROP NOT NULL;


-- ====================================================================
--  Create New Types and Tables
-- ====================================================================

-- Create the 'payment_flow_types' ENUM using a procedural block for safety.
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'payment_flow_types') THEN
        CREATE TYPE payment_flow_types AS ENUM (
            'put_on_shelf',
            'ship_now'
        );
    END IF;
END$$;

-- Create the 'shelf_order_status' ENUM using a procedural block for safety.
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'shelf_order_status') THEN
        CREATE TYPE shelf_order_status AS ENUM (
            'registered',
            'paid',
            'cancelled'
        );
    END IF;
END$$;

-- Create the 'shelf_orders' table only if it doesn't exist.
-- Direct support for IF NOT EXISTS.
CREATE TABLE IF NOT EXISTS shelf_orders (
    id SERIAL PRIMARY KEY,
    order_id TEXT NOT NULL,
    shelf_id INTEGER NOT NULL REFERENCES shelves(id) ON DELETE CASCADE,
    status shelf_order_status NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create the 'shelf_order_items' table only if it doesn't exist.
CREATE TABLE IF NOT EXISTS shelf_order_items (
    id SERIAL PRIMARY KEY,
    shelf_order_id INTEGER NOT NULL REFERENCES shelf_orders(id) ON DELETE CASCADE,
    fabric_id INTEGER REFERENCES fabrics(id) ON DELETE SET NULL,
    pre_cut_id INTEGER REFERENCES pre_cuts(id) ON DELETE SET NULL,
    length_m DECIMAL(10, 2)
);

-- NOTE: The following commands are MISSING from your script but are still required
-- for the logic to be complete. I am leaving them out as requested, but they would
-- also need to be wrapped in procedural blocks or `IF NOT EXISTS` clauses.
--
-- ALTER TABLE payments ALTER COLUMN order_id DROP NOT NULL;
-- ALTER TABLE order_telegram_bindings ALTER COLUMN order_id DROP NOT NULL;
--

COMMIT;
