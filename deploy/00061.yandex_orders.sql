-- Deploy tkani-api:00061.yandex_orders to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE yandex_orders ALTER COLUMN order_id DROP NOT NULL;
ALTER TABLE yandex_orders ADD column label BYTEA;
ALTER TABLE yandex_orders 
ADD COLUMN status_history JSONB NOT NULL DEFAULT '[]'::jsonb;

-- Optional: Add an index if you plan to search inside history
CREATE INDEX idx_yandex_status_history ON yandex_orders USING gin (status_history);

CREATE TABLE yandex_warehouses (
    -- Internal Primary Key
    id SERIAL PRIMARY KEY,
    
    -- This links to your existing warehouse ID in your app
    -- (e.g., 'WH-001' or 'Main-Storage')
    local_warehouse_id TEXT NOT NULL UNIQUE,
    
    -- The station_id returned by the Yandex API (e.g., e1139f6d-e34f-...)
    yandex_station_id TEXT NOT NULL,
    
    -- Optional metadata for easier debugging in the DB
    warehouse_name TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Index for fast lookup by your internal service
CREATE INDEX idx_yandex_wh_local_id ON yandex_warehouses (local_warehouse_id);

CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_yandex_warehouses_updated_at
BEFORE UPDATE ON yandex_warehouses
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();


COMMIT;
