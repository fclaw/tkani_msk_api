-- Deploy tkani-api:00020.new_function to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE fabrics ADD COLUMN daily_digest_id BIGINT NULL;
ALTER TABLE pre_cuts ADD COLUMN daily_digest_id BIGINT NULL;

-- === STEP 1: Update 'fabrics' ONLY if they are "Rolls" (have no pre-cuts) ===

UPDATE fabrics f
SET 
    daily_digest_id = dd.id
FROM 
    daily_digests AS dd
WHERE
    -- Join condition: match by date
    CAST(f.created_at AS date) = dd.announcement_date::date
    
    AND
    
    -- THE CRUCIAL CONDITION:
    -- Only perform this update if there are NO pre-cuts associated with this fabric.
    NOT EXISTS (
        SELECT 1 
        FROM pre_cuts pc 
        WHERE pc.fabric_id = f.id
    );

-- === STEP 2: Update 'pre_cuts' for fabrics that are sold as pre-cuts ===
-- This query remains the same as before. It will naturally only update
-- pre-cuts, and the first query will have ignored their parent fabrics.

UPDATE pre_cuts pc
SET 
    daily_digest_id = dd.id
FROM 
    fabrics AS f,
    daily_digests AS dd
WHERE
    -- Link pre-cut to its parent fabric
    pc.fabric_id = f.id
    AND
    -- Link that parent fabric's creation date to the digest's date
    CAST(f.created_at AS date) = dd.announcement_date::date;

COMMIT;