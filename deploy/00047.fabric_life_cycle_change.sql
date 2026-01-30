-- Deploy tkani-api:00047.fabric_life_cycle_change to pg

BEGIN;

-- XXX Add DDLs here.
-- This function will be executed by the trigger.
CREATE OR REPLACE FUNCTION notify_fabric_lifecycle_change()
RETURNS TRIGGER AS $$
BEGIN
    -- We only care about UPDATEs where the lifecycle state actually changed.
    IF (TG_OP = 'UPDATE' AND OLD.lifecycle IS DISTINCT FROM NEW.lifecycle) THEN

        -- Send a notification on the 'fabric_lifecycle_events' channel.
        -- The payload is a JSON object with the essential info our worker will need.
        PERFORM pg_notify(
            'fabric_lifecycle_events',
            jsonb_build_object(
                'fabric_name', NEW.name, 
                'new_lifecycle', NEW.lifecycle
            )::text
        );

    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Drop the trigger if it exists to make the script re-runnable.
DROP TRIGGER IF EXISTS fabric_lifecycle_change_trigger ON fabrics;

-- Create the trigger and attach it to the fabrics table.
-- The "OF lifecycle" is an optimization: the trigger only fires
-- if this specific column is part of the UPDATE statement.
CREATE TRIGGER fabric_lifecycle_change_trigger
AFTER UPDATE OF lifecycle ON fabrics
FOR EACH ROW
EXECUTE FUNCTION notify_fabric_lifecycle_change();

 CREATE TABLE temporary_notification_messages (
    id SERIAL PRIMARY KEY,
    -- The ID of the channel where the notification was posted.
    channel_id BIGINT NOT NULL,
    message_id BIGINT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    -- This is the crucial constraint for robustness.
    -- It ensures you can never insert a duplicate record for the
    -- exact same message in the exact same channel.
    UNIQUE (channel_id, message_id)
);

-- An index to make the janitor's cleanup query fast.
CREATE INDEX idx_temporary_notifications_messages_created_at ON temporary_notification_messages (created_at);

COMMIT;
