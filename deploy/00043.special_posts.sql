-- Deploy tkani-api:00043.special_posts to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TYPE special_post_type AS ENUM (
   'advertised',
   'regular',
   'on_sale',
   'clearance'
);



CREATE TABLE special_posts (
    id SERIAL PRIMARY KEY,

    -- We can use an ENUM to identify the type of summary post.
    -- For now, we only have one, but this allows for future expansion
    -- (e.g., 'NEW_ARRIVALS_INVITATION').
    post_type special_post_type NOT NULL DEFAULT 'regular',  -- 'CLEARANCE_INVITATION'

    message_id BIGINT NOT NULL,

    -- The timestamp when this post was last created or updated.
    posted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    is_active BOOLEAN NOT NULL DEFAULT TRUE,
        -- Enforce the "one active post per type" rule at the database level.
    UNIQUE (post_type, is_active) WHERE (is_active = TRUE)
);



ALTER TABLE fabrics DROP COLUMN IF EXISTS selling_price;
ALTER TABLE fabrics DROP COLUMN IF EXISTS daily_digests_id;
ALTER TABLE fabrics ADD COLUMN discount NUMERIC(5, 4) NOT NULL DEFAULT 0.0; -- in percentage

DROP TABLE IF EXISTS daily_digests;

COMMIT;
