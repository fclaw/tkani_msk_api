#!/bin/bash
set -e

# This script now uses docker-compose to manage the application stack.
# It assumes you have a docker-compose.yml file in this directory.

ENV_FILE="./.env"

if [ ! -f "$ENV_FILE" ]; then
    echo "Error: .env file not found!"
    exit 1
fi

# This line reads the .env file, removes comments (#) and empty lines,
# and then exports each variable.
export $(grep -v '^#' $ENV_FILE | xargs)

echo "🚀 Starting the Tkani API stack (API + Database)..."

docker-compose up -d db

# sqitch deploy "db:pg://{$POSTGRES_USER}:{$POSTGRES_PASSWORD}@localhost:5432/tkani_db"

# 'up -d' starts the services defined in docker-compose.yml in detached mode.
# '--build' is optional - it will rebuild the 'api' image if its Dockerfile has changed.
docker-compose up -d --build

echo "✅ Tkani API stack is up and running."