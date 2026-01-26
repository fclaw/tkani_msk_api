-- Deploy tkani-api:00046.shelf to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TYPE shelf_item_status AS ENUM (
    'ON_SHELF',          -- The item is currently being stored.
    'SHIPPED'            -- The item has been sent.
);


CREATE TABLE shelves (
    id SERIAL PRIMARY KEY,

    -- Your #1 field: The user identity tied to the bucket.
    telegram_user_id BIGINT UNIQUE NOT NULL,

    -- Your #2 & #3 fields: Info about the user who owns the bucket.
    user_initials TEXT,
    user_phone TEXT,

    -- Your #4 field: When the bucket itself was created.
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Your #5 field: When the first item was put in (for lifetime).
    -- This is managed by the application.
    first_item_added_at TIMESTAMPTZ,

    -- A 'last_updated_at' is also very useful for maintenance.
    last_updated_at TIMESTAMPTZ
);

-- Index for fast lookup by Telegram ID.
CREATE INDEX idx_shelves_telegram_user_id ON shelves (telegram_user_id);

CREATE TABLE shelf_items (
    id SERIAL PRIMARY KEY,

    -- This now points to the bucket ID.
    -- ON DELETE CASCADE means if you delete a user's shelf, all items in it are also deleted.
    shelf_id INTEGER NOT NULL REFERENCES shelves(id) ON DELETE CASCADE,

    -- When this specific item was put into the bucket.
    added_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    
    -- Status of the item (e.g., 'ON_SHELF', 'SHIPPED')
    status shelf_item_status NOT NULL DEFAULT 'ON_SHELF',
     
    -- fabric_id and length_m are bundled together
    fabric_id BIGINT NULL,
    length_m DECIMAL(10, 2) NULL,
    pre_cut_id BIGINT

);

-- Index for quickly finding all items in a given bucket.
CREATE INDEX idx_shelf_items_shelf_id ON shelf_items (shelf_id);

COMMIT;
