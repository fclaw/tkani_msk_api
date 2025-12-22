-- Deploy tkani-api:00019.new_function to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION notify_collage_maker()
RETURNS TRIGGER AS $$
DECLARE
    collage_urls TEXT[];
    all_fabric_names TEXT[];
BEGIN
    -- Fire only when the status transitions to 'ready'
    IF NEW.status = 'ready' AND OLD.status <> 'ready' THEN
    
       -- Use a single CTE to find the set of unique fabrics for the day.
        -- This is the "single source of truth" for the rest of the query.
        WITH unique_fabrics_today AS (
            SELECT DISTINCT ON (name)
                name,
                thumbnail_url,
                created_at -- We need this for consistent ordering
            FROM fabrics
            WHERE updated_at::date = NEW.created_at::date
              AND in_stock = TRUE
              AND is_sold = FALSE
            ORDER BY
                name, created_at DESC -- Pick the newest item for each unique name
        )
        -- Now, run both aggregations against this CTE in a single query.
        SELECT
            -- Aggregate 1: Get ALL unique names, ordered by creation time.
            array_agg(name ORDER BY created_at DESC),

            -- Aggregate 2: Get a random sample of up to 9 thumbnail URLs from the same set.
            (SELECT array_agg(thumbnail_url)
             FROM (
                SELECT thumbnail_url
                FROM unique_fabrics_today
                WHERE thumbnail_url IS NOT NULL
                ORDER BY random()
                LIMIT 9
             ) AS random_sample)
        INTO
            all_fabric_names,
            collage_urls
        FROM
            unique_fabrics_today;

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
