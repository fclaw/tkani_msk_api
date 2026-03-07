-- Deploy tkani-api:00059.cancel_order_event_func to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_cancel_event()
RETURNS TRIGGER AS $$
DECLARE
    v_sdek_uuid UUID;
    v_provider  TEXT;
    v_yandex_id TEXT;
BEGIN
    -- Only fire on 'cancelled' status transition
    IF (TG_OP = 'UPDATE' AND NEW.status = 'cancelled' AND OLD.status != 'cancelled') THEN
        
        -- 1. Identify Provider and fetch relevant UUID
        -- We check if the foreign key for sdek_orders is set
        IF NEW.sdek_order_id IS NOT NULL THEN
            SELECT order_uuid INTO v_sdek_uuid 
            FROM sdek_orders 
            WHERE id = NEW.sdek_order_id;
            
            v_yandex_id := NULL; -- Explicitly set to NULL since this is a SDEK order
            v_provider := 'sdek'; -- This must match your Haskell 'Providers' enum tag
        
        -- Future: Check if Yandex provider
        -- ELSIF NEW.yandex_order_id IS NOT NULL THEN
        --    v_provider := 'Yandex';
        --    v_sdek_uuid := NULL; -- Correctly explicitly set to NULL for Haskell Maybe
        
        ELSE
            v_provider := NULL;
        END IF;

        -- 2. Send notification only if a delivery provider was involved
        IF v_provider IS NOT NULL THEN
            PERFORM pg_notify(
                'order_cancel_events',
                jsonb_build_object(
                    'order_id', NEW.id::text,
                    'delivery_provider', to_json(v_provider),
                    'sdek_order_uuid', v_sdek_uuid,
                    'yandex_order_id', v_yandex_id
                )::text
            );
        END IF;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

COMMIT;
