-- Deploy tkani-api:00049.shelf_items_main_order to pg

BEGIN;

-- XXX Add DDLs here.
ALTER TABLE shelf_items ADD COLUMN main_order_id TEXT;

CREATE INDEX idx_shelf_items_main_order_id ON shelf_items(main_order_id);

ALTER TABLE shelves ADD status TEXT NOT NULL DEFAULT 'requested';

-- 2. Create the new submissions table
CREATE TABLE shelf_submissions (
    id SERIAL PRIMARY KEY,
    chat_id BIGINT NOT NULL,
    message_id BIGINT NOT NULL,
    -- Add the user_id for a clean link to the shelves table
    telegram_user_id BIGINT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    is_answered BOOLEAN NOT NULL DEFAULT FALSE,
    UNIQUE (chat_id, message_id)
);

-- Index for fast lookups
CREATE INDEX idx_shelf_submissions_user_answered ON shelf_submissions (telegram_user_id, is_answered);


CREATE OR REPLACE FUNCTION notify_shelf_status_change()
RETURNS TRIGGER AS $$
DECLARE
    -- A variable to hold the submission record we find.
    submission_record RECORD;
BEGIN
    -- We only care about UPDATEs where the status actually changed to a "final" state.
    -- This fires only on the transition into 'active' or 'waitlisted'.
    IF (TG_OP = 'UPDATE' AND OLD.status = 'requested' AND NEW.status IN ('active', 'waitlisted')) THEN

        -- Find the latest unanswered submission for this user (telegram_user_id).
        -- We assume 'shelves.id' can be linked to 'shelf_submissions' via a chat_id/user_id.
        -- Let's assume shelves.telegram_user_id is the link.
        SELECT * INTO submission_record
        FROM shelf_submissions
        WHERE chat_id = NEW.telegram_user_id AND is_answered = FALSE
        ORDER BY created_at DESC
        LIMIT 1;

        -- Check if we found an unanswered submission record.
        -- 'IF FOUND' is true if the SELECT INTO query returned a row.
        IF FOUND THEN
            -- ALL CONDITIONS MET!
            
            -- 1. Send the notification with all the necessary info.
            PERFORM pg_notify(
                'shelf_status_events',
                jsonb_build_object(
                    'shelf_id', NEW.id,
                    'telegram_user_id', NEW.telegram_user_id,
                    'new_status', NEW.status,
                    -- Include the submission details so the bot knows which message to reply to.
                    'reply_to_chat_id', submission_record.chat_id,
                    'reply_to_message_id', submission_record.message_id
                )::text
            );

            -- 2. CRITICAL: Mark this submission as answered to prevent re-triggering.
            --    This happens in the same transaction as the notification.
            UPDATE shelf_submissions
            SET is_answered = TRUE
            WHERE id = submission_record.id;
        END IF;

    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER shelf_status_change_trigger
AFTER UPDATE OF status ON shelves
FOR EACH ROW
EXECUTE FUNCTION notify_shelf_status_change();


COMMIT;
