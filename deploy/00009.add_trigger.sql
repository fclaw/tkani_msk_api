-- Deploy tkani-api:00009.add_trigger to pg

BEGIN;

-- XXX Add DDLs here.
-- Step 1: Create the function that sends the notification
CREATE OR REPLACE FUNCTION notify_collage_maker()
RETURNS TRIGGER AS $$
DECLARE
    -- 1. Declare a variable to hold our aggregated URLs
    random_urls TEXT[];
BEGIN
    -- We only fire the event if the status is changing TO 'ready'.
    IF NEW.status = 'ready' AND OLD.status <> 'ready' THEN
    
        -- 2. Execute a query to get 9 random, non-null thumbnail URLs
        SELECT 
            array_agg(thumbnail_url)
        INTO 
            random_urls
        FROM (
            SELECT thumbnail_url
            FROM fabrics
            WHERE thumbnail_url IS NOT NULL
            AND updated_at :: date = CURRENT_DATE
            -- FIX: Use ORDER BY random() for simplicity and correctness
            ORDER BY random()
            LIMIT 9
        ) AS random_sample;

        -- 3. Construct the final payload string
        -- We'll use a separator (e.g., a pipe '|') to separate the ID and the URL list
        -- Payload format: "digest_id|url1,url2,url3,..."
        PERFORM pg_notify(
            'collage_jobs',
            jsonb_build_object(
             'chat_id', NEW.warehouse_chat_id, 
             'message_id', NEW.warehouse_message_id,
             'final_draft', NEW.final_draft,
             'urls', COALESCE(random_urls, array[]::text[])) :: text,
             'fabric_names', ... <- here
        );
        
    END IF;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Step 2: Create the trigger on the 'daily_digests' table

CREATE TRIGGER daily_digest_ready_trigger
AFTER UPDATE ON daily_digests
FOR EACH ROW
EXECUTE FUNCTION notify_collage_maker();

COMMIT;
