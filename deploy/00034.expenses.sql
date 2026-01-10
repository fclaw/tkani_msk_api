-- Deploy tkani-api:00034.expenses to pg

BEGIN;

-- XXX Add DDLs here.
CREATE TABLE partners (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE, -- e.g., 'Sidorenkova', 'Yakovlev'
    is_active BOOLEAN DEFAULT TRUE
);
-- You could also have a 'company' row here or handle it separately.

CREATE TABLE expenses (
    id SERIAL PRIMARY KEY,
    amount NUMERIC(16, 2) NOT NULL,
    -- ... other expense details ...
    
    -- This links to a specific partner. Can be NULL if it was a company expense.
    paid_by_partner_id INT REFERENCES partners(id) ON DELETE SET NULL,

    -- A simple boolean flag to distinguish company vs. personal
    is_company_expense BOOLEAN NOT NULL DEFAULT FALSE,

    description Text,

    day DATE NOT NULL DEFAULT now()::date,

    -- Add a check constraint to ensure it's one or the other
    CONSTRAINT expense_payer_check CHECK (
        (is_company_expense = TRUE AND paid_by_partner_id IS NULL) OR
        (is_company_expense = FALSE AND paid_by_partner_id IS NOT NULL)
    )
);

CREATE OR REPLACE FUNCTION create_expense(
    p_amount DOUBLE PRECISION,
    p_partner_name TEXT, -- Can be NULL for a company expense
    p_description TEXT,
    p_day DATE
)
RETURNS BOOLEAN AS $$ -- Returns true on success, false on failure
DECLARE
    v_partner_id INT;
    v_is_company BOOLEAN;
BEGIN
    -- Determine if it's a company expense or a partner expense
    IF p_partner_name IS NULL THEN
        v_is_company := TRUE;
        v_partner_id := NULL;
    ELSE
        -- It's a partner expense. We must find the partner's ID.
        SELECT id INTO v_partner_id
        FROM partners
        WHERE name = p_partner_name AND is_active = TRUE;

        -- THIS IS THE KEY CHECK: If the subquery found no partner, the ID will be NULL.
        IF NOT FOUND THEN
            -- The partner does not exist or is not active. ABORT.
            RETURN FALSE;
        END IF;

        v_is_company := FALSE;
    END IF;

    -- If we get here, the data is valid. Proceed with the INSERT.
    INSERT INTO expenses (amount, paid_by_partner_id, is_company_expense, description, day)
    VALUES (p_amount, v_partner_id, v_is_company, p_description, p_day);

    RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

INSERT INTO partners (name) VALUES ('yakovlev');
INSERT INTO partners (name) VALUES ('sidorenkova');

COMMIT;
