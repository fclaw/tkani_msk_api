#!/bin/bash
set -e

# This script now uses docker-compose to manage the application stack.
# It assumes you have a docker-compose.yml file in this directory.

echo "🚀 Starting the Tkani API stack (API + Database)..."

USER=${1}
PASS=${2}

docker-compose up -d db

sqitch deploy "db:pg://{$USER}:{$PASS}@localhost:5432/tkani_db"

# 'up -d' starts the services defined in docker-compose.yml in detached mode.
# '--build' is optional - it will rebuild the 'api' image if its Dockerfile has changed.
docker-compose up -d --build

echo "✅ Tkani API stack is up and running."