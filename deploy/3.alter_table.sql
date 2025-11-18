-- Deploy tkani-api:3.alter_table to pg

BEGIN;

-- XXX Add DDLs here.
BEGIN;

-- This migration adds a unique 'article' number (SKU) to the 'fabrics' table.
-- The article will serve as the primary business-level identifier for a fabric.

-- Step 1: Add the new 'article' column.
-- We make it nullable at first so we can add it to a table with existing data.
ALTER TABLE fabrics
ADD COLUMN article TEXT;

-- Step 2 (Optional but Recommended): Backfill existing rows.
-- If you have existing fabrics, you need to give them a unique article number
-- before we can make the column NOT NULL and UNIQUE.
-- We can use the existing 'id' as a simple, unique starting point.
-- You should manually update these to their real article numbers later.
UPDATE fabrics
SET article = 'ART-' || id
WHERE article IS NULL;

-- Step 3: Now that all rows have a value, we can enforce the constraints.
-- Make the column required (NOT NULL).
ALTER TABLE fabrics
ALTER COLUMN article SET NOT NULL;

-- Add a UNIQUE constraint to ensure data integrity. This also automatically
-- creates an index on the 'article' column, making lookups by article very fast.
ALTER TABLE fabrics
ADD CONSTRAINT fabrics_article_unique UNIQUE (article);

-- Add a comment to document the purpose of the new column.
COMMENT ON COLUMN fabrics.article IS 'The unique article number (SKU) for the fabric.';

COMMIT;

COMMIT;
