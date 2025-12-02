-- Deploy tkani-api:00008.add_table to pg

BEGIN;

-- XXX Add DDLs here.
-- migration_create_announcements.sql
CREATE TYPE announcement_status AS ENUM ('draft', 'published', 'ready');

CREATE TABLE daily_announcements (
    id SERIAL PRIMARY KEY,
    
    -- The date this announcement is for
    announcement_date DATE NOT NULL UNIQUE,
    
    -- The "coordinates" of the draft post in the warehouse
    warehouse_chat_id BIGINT NOT NULL,
    warehouse_message_id BIGINT NOT NULL,
    
    -- The "coordinates" of the final post in the public channel
    public_chat_id BIGINT,
    public_message_id BIGINT,
    
    -- Lifecycle
    status announcement_status NOT NULL DEFAULT 'draft',
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    published_at TIMESTAMPTZ
);

COMMIT;
