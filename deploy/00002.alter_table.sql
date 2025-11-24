-- Deploy tkani-api:00002.alter_table to pg

BEGIN;

-- XXX Add DDLs here.
BEGIN;

-- 1. Add the column that auto-generates tokens
ALTER TABLE fabrics 
ADD COLUMN search_vector tsvector
GENERATED ALWAYS AS (
    -- Weight A (High Priority): Name of the fabric
    setweight(to_tsvector('russian', coalesce(description, '')), 'A') || 
    
    -- Weight B (Medium): The Article Number
    setweight(to_tsvector('simple', coalesce(article, '')), 'B') ||
    
    -- Weight C (Low): The Description / Tags
    -- We assume you might have a long description column, if so, map it here.
    -- For now mapping 'description' again as C just as example:
    setweight(to_tsvector('russian', coalesce(description, '')), 'C')
) STORED;

-- 2. Create a GIN Index (Makes search instant even with 1M rows)
CREATE INDEX idx_fabrics_search_vector ON fabrics USING GIN (search_vector);

COMMIT;

COMMIT;
