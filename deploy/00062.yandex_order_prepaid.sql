-- Deploy tkani-api:00062.yandex_order_prepaid to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE yandex_orders ADD COLUMN is_prepaid BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE yandex_orders ADD COLUMN prepaid_cost INTEGER; -- in kopecks
ALTER TABLE yandex_orders ADD COLUMN is_shipment_paid BOOLEAN NOT NULL DEFAULT FALSE;


CREATE TABLE shipment_payments (
    -- Internal unique ID for the payment attempt.
    id BIGSERIAL PRIMARY KEY,

    -- Foreign key linking back to the main order.
    order_id TEXT NOT NULL,

    parcel_order_id TEXT NOT NULL REFERENCES orders(id) ON DELETE CASCADE,

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

    -- A unique token to identify this payment attempt externally (e.g., in webhooks).
    token TEXT NOT NULL,

    chat_id BIGINT NOT NULL,
    message_id BIGINT NOT NULL,

    -- Make sure a provider's payment ID is unique for that provider.
    CONSTRAINT unique_shipment_provider_payment_id UNIQUE (provider, provider_payment_id)
);

-- Create a trigger to automatically update the 'updated_at' timestamp.
CREATE TRIGGER set_shipment_payments_updated_at
BEFORE UPDATE ON shipment_payments
FOR EACH ROW EXECUTE PROCEDURE set_updated_at_timestamp(); -- Assumes this function already exists

-- Add indexes for columns that will be frequently queried by the poller.
CREATE INDEX idx_shipment_payments_order_id ON shipment_payments(order_id);
CREATE INDEX idx_shipment_payments_parcel_order_id ON shipment_payments(parcel_order_id);
CREATE INDEX idx_shipment_payments_status ON shipment_payments(status);
CREATE INDEX idx_shipment_payments_provider_payment_id ON shipment_payments(provider_payment_id);

COMMIT;
