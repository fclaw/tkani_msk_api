-- Deploy tkani-api:00069.fabrics_composition to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE fabrics ADD COLUMN composition JSONB NOT NULL DEFAULT '{}';

CREATE OR REPLACE FUNCTION format_fabric_composition(comp jsonb)
RETURNS text AS $$
BEGIN
    IF comp IS NULL OR comp = '{}'::jsonb THEN
        RETURN '';
    END IF;

    RETURN (
        SELECT string_agg(value || '% ' || key, ', ' ORDER BY value::int DESC)
        FROM jsonb_each_text(comp)
    );
END;
$$ LANGUAGE plpgsql STABLE;


COMMIT;
