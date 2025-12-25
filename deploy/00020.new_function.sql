-- Deploy tkani-api:00020.new_function to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE fabrics ADD COLUMN daily_digests_id BIGINT NULL;
ALTER TABLE pre_cuts ADD COLUMN daily_digests_id BIGINT NULL;

UPDATE fabrics
SET 
    daily_digests_id = subquery.digest_id
FROM (
    -- This subquery finds the correct digest_id for each date
    SELECT
        id AS digest_id,
        announcement_date::date -- Cast to date to match the fabric's date
    FROM
        daily_digests
) AS subquery
WHERE
    -- The join condition: match the fabric's creation date to the digest's announcement day
    CAST(fabrics.created_at AS date) = subquery.announcement_date;

COMMIT;