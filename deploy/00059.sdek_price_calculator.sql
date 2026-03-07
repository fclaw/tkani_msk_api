-- Deploy tkani-api:00059.sdek_price_calculator to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_price_calculator()
RETURNS TRIGGER AS $$
DECLARE
    v_provider TEXT;
BEGIN
    -- 1. Determine the provider string
    IF NEW.sdek_order_id IS NOT NULL THEN
        v_provider := 'sdek';
    ELSIF NEW.yandex_order_id IS NOT NULL THEN -- Future-proof check
        v_provider := 'yandex';
    ELSE
        v_provider := NULL;
    END IF;

    -- === CASE 1: YAML INSERT ===
    -- Fired for new SDEK orders from the importer (non-bot, weight/link ready)
    IF TG_OP = 'INSERT' 
       AND NEW.is_bot = FALSE 
       AND NEW.actual_weight_grams IS NOT NULL 
       AND v_provider IS NOT NULL
    THEN
        PERFORM pg_notify(
            'price_calculation_jobs',
            jsonb_build_object(
              'order_id', NEW.id, 
              'is_bot', FALSE,
              'provider', v_provider
            )::text
        );
        
    END IF;
    
    -- === CASE 2: BOT UPDATE ===
    -- Fired when a bot-order hits 'paid' and data becomes ready
    IF TG_OP = 'UPDATE' 
       AND NEW.status = 'paid' 
       AND NEW.is_bot = TRUE
       AND NEW.actual_weight_grams IS NOT NULL 
       AND NEW.length IS NOT NULL 
       AND NEW.width  IS NOT NULL 
       AND NEW.height IS NOT NULL
       AND v_provider IS NOT NULL
       -- Fire if dimensions changed OR if weight was just filled in
       AND (
           OLD.length IS DISTINCT FROM NEW.length OR
           OLD.width  IS DISTINCT FROM NEW.width  OR
           OLD.height IS DISTINCT FROM NEW.height OR
           OLD.actual_weight_grams IS DISTINCT FROM NEW.actual_weight_grams
       )
    THEN
        PERFORM pg_notify(
            'price_calculation_jobs',
            jsonb_build_object(
              'order_id', NEW.id,
              'is_bot', TRUE,
              'provider', v_provider
            )::text
        );
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION notify_receipt_generator()
RETURNS TRIGGER AS $$
DECLARE
    v_receipt_uuid UUID;
BEGIN
    -- The trigger handles the transition: receipt_ready: FALSE -> TRUE
    IF TG_OP = 'UPDATE' 
       AND NEW.receipt_ready = TRUE 
       AND OLD.receipt_ready = FALSE
    THEN
        -- 1. Identify which table to fetch the receipt from
        IF NEW.sdek_order_id IS NOT NULL THEN
            -- Fetch the UUID from sdek_orders
            SELECT receipt_uuid INTO v_receipt_uuid 
            FROM sdek_orders 
            WHERE id = NEW.sdek_order_id;
            
        -- ELSIF NEW.yandex_order_id IS NOT NULL THEN
        --    SELECT receipt_uuid INTO v_receipt_uuid FROM yandex_orders WHERE id = NEW.yandex_order_id;
        
        ELSE
            v_receipt_uuid := NULL;
        END IF;

        -- 2. Notify the worker only if we found a receipt context
        IF v_receipt_uuid IS NOT NULL THEN
            PERFORM pg_notify(
              'receipt_jobs',
              jsonb_build_object(
                'receipt_uuid', v_receipt_uuid,
                'order_id', NEW.id,
                'customer', NEW.customer_full_name
              )::text
            );
        END IF;
        
    END IF;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;


COMMIT;
