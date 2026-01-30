-- Deploy tkani-api:00048.order_status_pickuo_failure to pg

BEGIN;

-- XXX Add DDLs here.
DO $$
BEGIN
    -- This block checks if the 'pickup_failed' value already exists in the 'order_status' enum.
    -- If it does not exist, the ALTER TYPE command is executed.
    -- If it already exists, the block does nothing, preventing an error.
    IF NOT EXISTS (SELECT 1 FROM pg_enum WHERE enumlabel = 'pickup_failed' AND enumtypid = 'order_status'::regtype) THEN
        ALTER TYPE order_status ADD VALUE 'pickup_failed';
    END IF;
END
$$;

COMMIT;
