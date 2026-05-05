-- Deploy tkani-api:00072.bonuses to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TABLE bonuses (
    id SERIAL PRIMARY KEY,
    telegram_user_id BIGINT NOT NULL,
    points INT NOT NULL DEFAULT 0,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),
    CONSTRAINT unique_telegram_user_id UNIQUE (telegram_user_id)
);

CREATE TABLE added_bonuses (
    id SERIAL PRIMARY KEY,
    payment_id BIGINT NOT NULL REFERENCES payments(id) ON DELETE CASCADE,
    telegram_user_id BIGINT NOT NULL,
    points INT NOT NULL DEFAULT 0,
    created_at TIMESTAMPTZ DEFAULT NOW()
);


ALTER TABLE payments ADD COLUMN bonuses INT DEFAULT 0;

-- event
CREATE OR REPLACE FUNCTION notify_bonuses_added()
RETURNS TRIGGER AS $$
BEGIN
    IF (TG_OP = 'UPDATE' AND 
        OLD.status IS DISTINCT FROM 'confirmed' AND 
        NEW.status = 'confirmed')
    THEN
        PERFORM pg_notify(
            'bonuses_added_events',
            jsonb_build_object(
              'payment_id', NEW.id::BIGINT
            )::text
        );
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;



CREATE TRIGGER bonuses_added_trigger
AFTER UPDATE ON payments -- Added INSERT here
FOR EACH ROW
EXECUTE FUNCTION notify_bonuses_added();


COMMIT;
