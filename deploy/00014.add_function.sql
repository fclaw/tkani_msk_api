-- Deploy tkani-api:00014.add_function to pg

BEGIN;

-- XXX Add DDLs here.
-- 1. Create a sequence to generate unique numbers for articles.
--    We'll start it at 1, but you could start it higher if you have existing articles.
CREATE SEQUENCE fabric_article_seq START 1;

-- 2. Create a function that formats this number into your ART-00000 format.
--    This function takes no arguments and returns the next formatted article ID.
CREATE OR REPLACE FUNCTION next_fabric_article()
RETURNS TEXT AS $$
DECLARE
    next_val BIGINT;
BEGIN
    -- Get the next value from our sequence
    next_val := nextval('fabric_article_seq');
    -- Format it with leading zeros to 5 digits (e.g., 1 -> '00001', 123 -> '00123')
    -- and prepend 'ART-'.
    RETURN 'ART-' || lpad(next_val::TEXT, 5, '0');
END;
$$ LANGUAGE plpgsql;

COMMIT;
