-- Deploy tkani-api:00017.patch_function to pg

BEGIN;

-- XXX Add DDLs here.
-- migration_create_synchronized_article_sequence.sql

DROP SEQUENCE IF EXISTS fabric_article_seq;

-- 1. Create the sequence, starting at 1 for now.
CREATE SEQUENCE fabric_article_seq START 1;

-- 2. Create the main function that will be used by your application.
CREATE OR REPLACE FUNCTION next_fabric_article()
RETURNS TEXT AS $$
DECLARE
    next_val BIGINT;
BEGIN
    next_val := nextval('fabric_article_seq');
    -- Format: ART-00001
    RETURN 'ART-' || lpad(next_val::TEXT, 5, '0');
END;
$$ LANGUAGE plpgsql;

-- 3. Synchronization Step: Set the sequence to the correct current value.
--    This block of code runs only once, during the migration.
DO $$
DECLARE
    max_article_num BIGINT;
BEGIN
    -- Find the highest number from existing articles matching the 'ART-NNNNN' pattern.
    -- The regex 'ART-([0-9]+)$' captures the numeric part.
    -- COALESCE handles the case where the table is empty.
    SELECT COALESCE(MAX(CAST(substring(article from 'ART-([0-9]+)$') AS BIGINT)), 0)
    INTO max_article_num
    FROM fabrics
    WHERE article ~ '^ART-[0-9]+$';

    -- Set the sequence's current value.
    -- The 'is_called' parameter is false, meaning the NEXT call to nextval()
    -- will return max_article_num + 1.
    IF max_article_num > 0 THEN
        PERFORM setval('fabric_article_seq', max_article_num, true);
    END IF;
END $$;

COMMIT;
