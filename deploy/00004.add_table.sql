-- migration_create_payments_table.sql

BEGIN;

-- First, create an ENUM for the payment providers you support.
CREATE TYPE payment_provider AS ENUM (
    'tinkoff'
    -- You can add more later, e.g., 'yookassa', 'stripe'
);

-- Then, create an ENUM for the payment status. This is more robust than text.
CREATE TYPE payment_status AS ENUM (
    'pending',      -- Link created, waiting for user to pay
    'confirmed',    -- Payment successful
    'rejected',     -- Payment failed (e.g., card declined)
    'cancelled',    -- Payment cancelled by user or timeout
    'error'         -- An unexpected error occurred
);

-- Now, create the main 'payments' table.
CREATE TABLE payments (
    -- Internal unique ID for the payment attempt.
    id BIGSERIAL PRIMARY KEY,

    -- Foreign key linking back to the main order.
    order_id TEXT NOT NULL REFERENCES orders(id) ON DELETE CASCADE,

    -- Information about the payment provider.
    provider payment_provider NOT NULL,
    
    -- The unique ID for this transaction from the provider's system (e.g., Tinkoff's PaymentId).
    provider_payment_id TEXT NOT NULL,

    -- The URL the customer is sent to.
    payment_url TEXT,

    -- The status of this specific payment attempt.
    status payment_status NOT NULL DEFAULT 'pending',

    -- Total amount for this payment attempt, in kopecks.
    amount INT NOT NULL,

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),

    -- Optional error message for debugging failed payments.
    error TEXT,

    -- Make sure a provider's payment ID is unique for that provider.
    CONSTRAINT unique_provider_payment_id UNIQUE (provider, provider_payment_id)
);

-- Create a trigger to automatically update the 'updated_at' timestamp.
CREATE TRIGGER set_payments_updated_at
BEFORE UPDATE ON payments
FOR EACH ROW EXECUTE PROCEDURE set_updated_at_timestamp(); -- Assumes this function already exists

-- Add indexes for columns that will be frequently queried by the poller.
CREATE INDEX idx_payments_order_id ON payments(order_id);
CREATE INDEX idx_payments_status ON payments(status);
CREATE INDEX idx_payments_provider_payment_id ON payments(provider_payment_id);

COMMIT;