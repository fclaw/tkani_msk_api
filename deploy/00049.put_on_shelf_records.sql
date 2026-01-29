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
