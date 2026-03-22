-- Deploy tkani-api:00062.yandex_order_prepaid to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE yandex_orders ADD COLUMN is_prepaid BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE yandex_orders ADD COLUMN prepaid_cost INTEGER; -- in kopecks
ALTER TABLE yandex_orders ADD COLUMN is_shipment_paid BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE yandex_orders ADD COLUMN delivery_days INTEGER;

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


CREATE OR REPLACE FUNCTION notify_shipment_payment_confirmed()
RETURNS TRIGGER AS $$
DECLARE
    v_delivery_days INT; -- Define a variable to hold the joined data
BEGIN
    -- Only fire when status transitions to 'confirmed'
    IF (OLD.status IS DISTINCT FROM 'confirmed' AND NEW.status = 'confirmed') THEN
        
        -- 1. Fetch the estimated delivery days from the joined tables
        SELECT yo.delivery_days INTO v_delivery_days
        FROM orders AS o
        INNER JOIN yandex_orders AS yo ON o.yandex_order_id = yo.id
        WHERE o.id = NEW.parcel_order_id;

        -- 2. Send notification with the extra field
        PERFORM pg_notify(
            'shipment_payment_events',
            jsonb_build_object(
                'parcel_order_id', NEW.parcel_order_id,
                'amount',          NEW.amount,
                'days',            COALESCE(v_delivery_days, 0) -- Safety fallback
            )::text
        );
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER shipment_payment_confirmed_trigger
AFTER UPDATE ON shipment_payments
FOR EACH ROW
EXECUTE FUNCTION notify_shipment_payment_confirmed();

COMMIT;
