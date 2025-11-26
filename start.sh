#!/bin/bash
# This script starts the full stack: Postgres DB + Haskell API.
# It builds the app, starts DB, waits for readiness, migrates, and launches the server.

# Immediately exit if any command fails.
set -e

# --- Configuration & Path Setup ---
echo "⚙️  Setting up environment..."

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
PROJECT_ROOT="$SCRIPT_DIR"

ENV_FILE="$PROJECT_ROOT/.env"
COMPOSE_FILE="$PROJECT_ROOT/docker-compose.yml"
IMAGE_NAME="tkani-api:latest"

# --- Pre-run Checks ---
if [ ! -f "$ENV_FILE" ]; then
    echo "❌ Error: Environment file not found at ${ENV_FILE}"
    exit 1
fi

if [ ! -f "$COMPOSE_FILE" ]; then
    echo "❌ Error: Docker compose file not found at ${COMPOSE_FILE}"
    exit 1
fi

# Load env vars for the script (used for sqitch/postgres readiness check)
export $(grep -v '^#' $ENV_FILE | xargs)

# --- Step 1: Build the Haskell Docker Image ---
# We build before starting anything to ensure we aren't running old code.
# echo ""
# echo "🐳 1. Building Haskell API Docker Image..."
# # We use the flags we optimized earlier (cache friendly)
# docker build -t "$IMAGE_NAME" "$PROJECT_ROOT"

# --- Step 2: Start Database ---
echo ""
echo "🚀 2. Launching Database..."
# Start ONLY the database first. We don't want the API crashing while waiting for migrations.
docker-compose -f "$COMPOSE_FILE" up -d db

# --- Step 3: Wait for DB Readiness ---
echo -n "⏳ 3. Waiting for database connection..."
until docker-compose -f "$COMPOSE_FILE" exec -T db pg_isready -U "$POSTGRES_USER" -d "$POSTGRES_DB" -q; do
  >&2 echo -n "."
  sleep 1
done
echo " [OK]"

# --- Step 4: Run Migrations ---
echo ""
echo "🔄 4. Applying Sqitch Migrations..."
# Note: Sqitch runs on the HOST, connecting to localhost:5432
export PGUSER=$POSTGRES_USER
export PGPASSWORD=$POSTGRES_PASSWORD

# Ensure we are in root for sqitch.conf resolution
(cd "$PROJECT_ROOT" && sqitch deploy "db:pg://localhost:5432/$POSTGRES_DB")

# # --- Step 5: Start API ---
# echo ""
# echo "🚀 5. Launching Backend API..."
# # Now that DB is migrated and ready, start the API service defined in compose
# docker-compose -f "$COMPOSE_FILE" up -d api

echo ""
echo "✅ Full stack is up!"
echo "   📊 Database: Running (Port 5432)"
# echo "   💻 API:      Running (Port "$API_EXTERNAL_PORT")"
echo "   📝 Logs:     'docker-compose logs -f api'"