-- Deploy tkani-api:00016.add_function to pg

BEGIN;

-- XXX Add DDLs here.
CREATE OR REPLACE FUNCTION search_fabrics_paginated(
    search_query TEXT,
    page_limit INT,
    page_offset INT,
    metre_threshold DOUBLE PRECISION
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
            f.available_length_m >= CAST($4 :: DOUBLE PRECISION AS NUMERIC) AND
            (
                f.search_vector @@ to_tsquery('russian', $1 :: TEXT)
                OR f.article ILIKE ($1 :: TEXT || '%')
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
            CAST(f.total_length_m AS INT) = 0 AND
            CAST(f.available_length_m AS INT) = 0 AND
            (
                f.search_vector @@ to_tsquery('russian', $1 :: TEXT)
                OR f.article ILIKE ($1 :: TEXT || '%')
            )
    )
    ORDER BY updated_at DESC
    LIMIT $2 :: INT
    OFFSET $3 :: INT)
    SELECT res.total_count :: int8, res.teaser_json :: jsonb FROM search_result AS res;
END;
$$ LANGUAGE plpgsql;

COMMIT;
