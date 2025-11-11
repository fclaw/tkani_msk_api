#!/bin/bash
# This script starts and prepares the PostgreSQL database container.
# It starts the service, waits for it to be healthy, and applies migrations.

# Immediately exit if any command fails.
set -e

# --- Configuration & Path Setup ---
echo "⚙️  Setting up environment..."

# Determine the absolute path to the project's root directory
# (one level up from the script's directory).
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJECT_ROOT="$SCRIPT_DIR"

# Define paths to the necessary configuration files.
ENV_FILE="$PROJECT_ROOT/.env"
COMPOSE_FILE="$PROJECT_ROOT/docker-compose.yml"

# --- Pre-run Checks ---
# Check if the .env file exists.
if [ ! -f "$ENV_FILE" ]; then
    echo "❌ Error: Environment file not found at ${ENV_FILE}"
    exit 1
fi

# Check if the docker-compose.yml file exists.
if [ ! -f "$COMPOSE_FILE" ]; then
    echo "❌ Error: Docker compose file not found at ${COMPOSE_FILE}"
    exit 1
fi

# Source the .env file to load variables like POSTGRES_USER and POSTGRES_DB
# for use in this script.
export $(grep -v '^#' $ENV_FILE | xargs)


# --- The Main Workflow ---

echo "🚀 Starting the PostgreSQL database service..."

# Step 1: Start the database container in the background.
# The '-f' flag explicitly points to our compose file.
echo "1. Launching container 'tkani-db' via docker-compose..."
docker-compose -f "$COMPOSE_FILE" up -d db

# Step 2: Wait for the database to become healthy.
# This is much more reliable than a simple 'sleep'. The loop continues
# until 'pg_isready' returns a success code (0).
echo -n "2. Waiting for database to accept connections..."
until docker-compose -f "$COMPOSE_FILE" exec -T db pg_isready -U "$POSTGRES_USER" -d "$POSTGRES_DB" -q; do
  >&2 echo -n "." # Print a dot for progress
  sleep 1
done
echo " [OK]" # Print a final confirmation

# Step 3: Apply database migrations using Sqitch.
# We run this from the project root so sqitch can find its config files.
echo "3. Applying database migrations..."
# The PGUSER and PGPASSWORD environment variables are automatically used by sqitch.
# Or you can be explicit like before.
export PGUSER=$POSTGRES_USER
export PGPASSWORD=$POSTGRES_PASSWORD
sqitch deploy "db:pg://localhost:5432/$POSTGRES_DB"
(cd "$PROJECT_ROOT" && sqitch deploy "db:pg://localhost:5432/$POSTGRES_DB")

echo "✅ Database is up, running, and migrated."