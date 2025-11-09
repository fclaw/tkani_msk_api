#!/bin/bash
set -e

# --- Configuration ---
IMAGE_NAME="tkani-api"

# --- Versioning Logic ---
GIT_HASH=$(git rev-parse --short HEAD)

if ! git diff-index --quiet HEAD --; then
    echo "⚠️  Warning: Uncommitted changes detected. Appending '-dirty' to tag."
    GIT_HASH="${GIT_HASH}-dirty"
fi

VERSION=$GIT_HASH

# --- Build Logic ---
echo "▶️  Building Docker image: ${IMAGE_NAME}:${VERSION}"

# Define the port to build the image with.
# You could even get this from a command-line argument.
API_INTERNAL_PORT=8080

# Pass the argument during the build
docker build \
    --build-arg APP_PORT=${API_INTERNAL_PORT} \
    -t "${IMAGE_NAME}:${VERSION}" .

echo "✅ Successfully built image: ${IMAGE_NAME}:${VERSION}"
docker tag "${IMAGE_NAME}:${VERSION}" "${IMAGE_NAME}:latest"