-- Deploy tkani-api:00018.new_function to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_collage_maker()
RETURNS TRIGGER AS $$
DECLARE
    -- Variable for the 9 random image URLs for the collage
    collage_urls TEXT[];
    -- Variable for ALL fabric names for the text body
    all_fabric_names TEXT[];
BEGIN
    -- Fire only when the status transitions to 'ready'
    IF NEW.status = 'ready' AND OLD.status <> 'ready' THEN
    
        -- === Step 1: Get ALL fabric names for the day ===
        -- There's no LIMIT here. We get every name.
        SELECT 
            array_agg(name ORDER BY created_at DESC) -- Order them for a predictable list
        INTO 
            all_fabric_names
        FROM fabrics
        WHERE updated_at::date = NEW.created_at::date;
        
        -- === Step 2: Get up to 9 random THUMBNAIL URLs for the collage ===
        -- This is a separate query.
        SELECT 
            array_agg(thumbnail_url)
        INTO 
            collage_urls
        FROM (
            SELECT thumbnail_url
            FROM fabrics
            WHERE thumbnail_url IS NOT NULL
              AND updated_at::date = NEW.created_at::date
            ORDER BY random()
            LIMIT 9
        ) AS random_sample;

        -- === Step 3: Construct and send the final JSON payload ===
        PERFORM pg_notify(
            'collage_jobs',
            jsonb_build_object(
                'digest_id', NEW.id,
                'chat_id', NEW.warehouse_chat_id, 
                'message_id', NEW.warehouse_message_id,
                'final_draft', NEW.final_draft,
                'urls', COALESCE(collage_urls, '{}'::text[]),
                'fabric_names', COALESCE(all_fabric_names, '{}'::text[])
            )::text
        );
        
    END IF;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

COMMIT;
