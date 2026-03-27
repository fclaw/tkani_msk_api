-- Deploy tkani-api:00065.fabrci_hash.sql to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE fabrics ADD COLUMN hash BIGINT;

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
                'new_lifecycle', NEW.lifecycle,
                'hash', NEW.hash
            )::text
        );

    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

COMMIT;
