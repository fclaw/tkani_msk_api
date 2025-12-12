-- Deploy tkani-api:00013.add_columns to pg

BEGIN;

-- XXX Add DDLs here.
-- migration_add_search_visibility.sql
ALTER TABLE fabrics
ADD COLUMN is_searchable BOOLEAN NOT NULL DEFAULT TRUE;

-- Add an index to make search queries faster
CREATE INDEX idx_fabrics_is_searchable ON fabrics(is_searchable);


-- migration_add_search_visibility.sql
ALTER TABLE pre_cuts
ADD COLUMN is_searchable BOOLEAN NOT NULL DEFAULT TRUE;

-- Add an index to make search queries faster
CREATE INDEX idx_pre_cuts_is_searchable ON pre_cuts(is_searchable);

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION search_fabrics_paginated(
    search_query TEXT,
    page_limit INT,
    page_offset INT
)
-- The function will return a table with these two columns
RETURNS TABLE(total_count BIGINT, teaser_json JSONB) AS $$
BEGIN
    RETURN QUERY
    -- Your existing, correct SQL query goes here
    WITH search_result AS (
    (
        SELECT
            f.updated_at,
            COUNT(*) OVER() AS total_count,
            jsonb_build_object(
                'id', f.id,
                'name', f.name,
                'article', f.article,
                'type', 'roll',
                'price', f.price_per_meter,
                'thumbnail_url', f.thumbnail_url
            ) :: jsonb AS teaser_json
        FROM 
            fabrics AS f
        WHERE
            f.is_searchable AND
            f.in_stock = TRUE AND 
            f.is_sold = FALSE AND
            CAST(f.total_length_m AS int4) > 0 AND
            CAST(f.available_length_m AS int4) > 0 AND
            (
                f.search_vector @@ to_tsquery('russian', $1 :: text)
                OR f.article ILIKE ($1 :: text || '%')
            )
    )

    UNION ALL

    (
        SELECT
            f.updated_at,
            COUNT(*) OVER() AS total_count,
            jsonb_build_object(
                'id', f.id,
                'pre_cut_id', pc.id,
                'name', f.name || ' (отрез ' || pc.length_m || 'м)',
                'article', f.article,
                'type', 'pre_cut',
                'price', pc.price_rub,
                'thumbnail_url', f.thumbnail_url
            ) :: jsonb AS teaser_json
        FROM 
            pre_cuts AS pc
        JOIN 
            fabrics AS f ON pc.fabric_id = f.id
        WHERE
            pc.is_searchable AND
            pc.in_stock = TRUE AND
            CAST(f.total_length_m AS int4) = 0 AND
            CAST(f.available_length_m AS int4) = 0 AND
            (
                f.search_vector @@ to_tsquery('russian', $1 :: text)
                OR f.article ILIKE ($1 :: text || '%')
            )
    )
    ORDER BY updated_at DESC
    LIMIT $2 :: int4
    OFFSET $3 :: int4)
    SELECT res.total_count :: int8, res.teaser_json :: jsonb FROM search_result AS res;
END;
$$ LANGUAGE plpgsql;

ALTER TABLE pre_cuts
ADD CONSTRAINT unique_fabric_precut_spec
UNIQUE (fabric_id, length_m, price_rub);

COMMIT;
