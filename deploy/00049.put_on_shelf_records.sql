-- Deploy tkani-api:00049.put_on_shelf_records to pg

BEGIN;

-- XXX Add DDLs here.

ALTER TABLE payments ALTER COLUMN order_id DROP NOT NULL;
ALTER TABLE payments ADD COLUMN shelf_order_id TEXT;

-- Step 1: Ensure data integrity by requiring at least one ID to be present.
ALTER TABLE payments
ADD CONSTRAINT payments_has_at_least_one_order_id
CHECK (order_id IS NOT NULL OR shelf_order_id IS NOT NULL);


ALTER TABLE order_telegram_bindings ADD COLUMN shelf_order_id TEXT;
ALTER TABLE order_telegram_bindings DROP CONSTRAINT order_telegram_bindings_pkey;
ALTER TABLE order_telegram_bindings ALTER COLUMN order_id DROP NOT NULL;

ALTER TABLE order_telegram_bindings
ADD CONSTRAINT order_telegram_bindings_has_at_least_one_order_id
CHECK (order_id IS NOT NULL OR shelf_order_id IS NOT NULL);

-- Step 2: Improve query performance for lookups by the new ID.
CREATE INDEX idx_payments_shelf_order_id ON payments(shelf_order_id);
CREATE INDEX idx_payments_order_id ON payments(order_id);
CREATE INDEX idx_order_telegram_bindings_shelf_order_id ON order_telegram_bindings(shelf_order_id);
CREATE INDEX idx_order_telegram_bindings_order_id ON order_telegram_bindings(order_id);

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


COMMIT;
