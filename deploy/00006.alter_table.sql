-- Deploy tkani-api:00002.alter_table to pg

BEGIN;

ALTER TABLE fabrics DROP COLUMN IF EXISTS search_vector;

-- 1. Add the column that auto-generates tokens
ALTER TABLE fabrics 
ADD COLUMN search_vector tsvector
GENERATED ALWAYS AS (
    -- Weight A (High Priority): Name of the fabric
    setweight(to_tsvector('russian', coalesce(name, '')), 'A') ||

    -- Weight B (High Priority): Description of the fabric
    setweight(to_tsvector('russian', coalesce(description, '')), 'B') || 
    
    -- Weight C (Medium): The Article Number
    setweight(to_tsvector('simple', coalesce(article, '')), 'C')
) STORED;

-- 2. Create a GIN Index (Makes search instant even with 1M rows)
CREATE INDEX idx_fabrics_search_vector ON fabrics USING GIN (search_vector);

COMMIT;
