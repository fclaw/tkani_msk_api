#!/bin/bash
set -e

echo "🚀 Deploying Tkani API on Server..."

echo "📥 Pulling images..."
docker compose pull

echo "🐘 Starting Database..."
docker compose up -d db

echo "⏳ Waiting for Database health..."
until [ "`docker inspect -f {{.State.Health.Status}} tkani-db`" == "healthy" ]; do
    sleep 1;
done;

# --- NEW: Run Migrations via Docker ---
echo "🔄 Running Database Migrations..."
# run --rm means: "Start it, do the work, then delete the container immediately"
docker compose run --rm migrator

echo "🤖 Starting API..."
docker compose up -d api nginx

echo "🧹 Cleanup..."
docker image prune -f

echo "🎉 Done!"