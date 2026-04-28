-- Deploy tkani-api:00070.money_transfer to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TABLE IF NOT EXISTS tinkoff_ruble_transfers (
    id SERIAL PRIMARY KEY,
    transfer_id TEXT NOT NULL UNIQUE,
    amount NUMERIC(10, 2) NOT NULL,
    agent TEXT NOT NULL,
    status TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

COMMIT;
