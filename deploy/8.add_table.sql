-- Deploy tkani-api:8.add_table to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TABLE order_telegram_bindings (
    -- 1. The Foreign Key linking to the main order
    -- Assuming your main table is 'orders' and id is 'text' or 'uuid'
    order_id TEXT PRIMARY KEY REFERENCES orders(id) ON DELETE CASCADE,
    
    -- 2. Telegram Data (BIGINT is mandatory for ChatID)
    chat_id BIGINT NOT NULL,
    message_id BIGINT NOT NULL,
    
    -- 3. Metadata
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

COMMIT;
