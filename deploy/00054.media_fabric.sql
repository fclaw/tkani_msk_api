-- Deploy tkani-api:00054.media_fabric to pg

BEGIN;


CREATE OR REPLACE FUNCTION check_fabric_media_parent()
RETURNS TRIGGER AS $$
BEGIN
    -- This function is executed for each row being inserted or updated.
    -- The 'NEW' variable refers to the row data that is about to be inserted.

    -- Case 1: The fabric_type is 'roll'.
    IF NEW.fabric_type = 'roll' THEN
        -- Check if the fabric_parent_id exists in the 'fabrics' table.
        IF NOT EXISTS (SELECT 1 FROM fabrics WHERE id = NEW.fabric_parent_id) THEN
            -- If it doesn't exist, raise an exception with a clear error message.
            -- This will cause the entire INSERT/UPDATE to fail and roll back.
            RAISE EXCEPTION 'Invalid fabric_parent_id: ID % does not exist in the fabrics table for type roll', NEW.fabric_parent_id;
        END IF;

    -- Case 2: The fabric_type is 'pre_cut'.
    ELSIF NEW.fabric_type = 'pre_cut' THEN
        -- Check if the fabric_parent_id exists in the 'pre_cuts' table.
        IF NOT EXISTS (SELECT 1 FROM pre_cuts WHERE id = NEW.fabric_parent_id) THEN
            RAISE EXCEPTION 'Invalid fabric_parent_id: ID % does not exist in the pre_cuts table for type pre_cut', NEW.fabric_parent_id;
        END IF;

    -- (Optional) Case 3: Handle any unknown fabric_type values.
    ELSE
        RAISE EXCEPTION 'Invalid fabric_type: ''%'' is not a valid type (must be ''roll'' or ''pre_cut'')', NEW.fabric_type;
    END IF;

    -- If all checks pass, allow the operation to proceed by returning NEW.
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Your table schema
CREATE TABLE fabric_media (
  id SERIAL PRIMARY KEY,
  fabric_parent_id BIGINT NOT NULL, -- Changed from allowing NULL
  fabric_type TEXT NOT NULL,
  telegram_file_id TEXT NOT NULL,
  media_type TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Drop the trigger if it already exists to make the script re-runnable.
DROP TRIGGER IF EXISTS validate_fabric_media_parent_trigger ON fabric_media;

-- Attach the trigger to the table.
-- It will execute our function *before* every INSERT or UPDATE.
CREATE TRIGGER validate_fabric_media_parent_trigger
BEFORE INSERT OR UPDATE ON fabric_media
FOR EACH ROW
EXECUTE FUNCTION check_fabric_media_parent();

COMMIT;
