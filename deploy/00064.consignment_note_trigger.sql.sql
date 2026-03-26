-- Deploy tkani-api:00064.consignment_note_trigger.sql to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_consignment_ready()
RETURNS TRIGGER AS $$
BEGIN
    -- CASE 1: New order inserted already in 'paid' status (e.g. Shelf Order)
    -- CASE 2: Existing order transitioned to 'paid' status (Transition)
    IF (TG_OP = 'INSERT' AND NEW.status = 'paid') OR
       (TG_OP = 'UPDATE' AND OLD.status IS DISTINCT FROM 'paid' AND NEW.status = 'paid') 
    THEN
        PERFORM pg_notify(
            'consignment_note_events',
            jsonb_build_object(
              'order_id', NEW.id::text
            )::text
        );
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS consignment_list_trigger ON orders;

CREATE TRIGGER consignment_list_trigger
AFTER INSERT OR UPDATE ON orders -- Added INSERT here
FOR EACH ROW
EXECUTE FUNCTION notify_consignment_ready();

COMMIT;
