#!/bin/bash
# This script creates a new fabric by calling the tkani-api PUT /fabric endpoint.

# Exit immediately if a command fails.
set -e

# --- Configuration ---
# The base URL of your running API service.
API_URL="http://localhost:8080"

# --- Argument Parsing ---
# Check if the correct number of arguments are provided.
if [ "$#" -ne 3 ]; then
    echo "❌ Error: Invalid number of arguments."
    echo "Usage: ./new_fabric.sh <description> <price_per_meter> <total_length_m>"
    echo ""
    echo "Example:"
    echo './new_fabric.sh "Luxurious black suiting cashmere" 150 10.5'
    exit 1
fi

# Assign arguments to variables for clarity
DESCRIPTION="$1"
PRICE_PER_METER=$2
TOTAL_LENGTH_M=$3

# --- Main Logic ---

echo "🚀 Preparing to create new fabric:"
echo "   - DESCRIPTION: ${DESCRIPTION}"

# 1. Construct the JSON payload using 'jq'.
#    This is much safer than trying to build the JSON string manually.
#    '--arg' handles strings, '--argjson' handles numbers/booleans.
JSON_PAYLOAD=$(jq -n \
                  --arg description "$DESCRIPTION" \
                  --argjson price "$PRICE_PER_METER" \
                  --argjson length "$TOTAL_LENGTH_M" \
                  '{
                    "fiDescription": $description,
                    "fiTotalLengthM": $length,
                    "fiPricePerMeter": $price,
                    "fiAvailableLengthM": $length
                  }')

echo "📦 Generated JSON Payload:"
echo "$JSON_PAYLOAD" | jq . # Pretty-print the JSON for verification

# 2. Send the PUT request using 'curl'.
echo "📡 Sending POST request to ${API_URL}/fabric..."
RESPONSE=$(curl -s -X POST "${API_URL}/fabric" \
     -H "Content-Type: application/json" \
     -d "$JSON_PAYLOAD" -v)

# 3. Check the response.
echo "✅ Server Response:"
# Check if the response contains "error". A simple but effective check.
if echo "$RESPONSE" | grep -q "error"; then
    echo "❌ Error from server:"
    echo "$RESPONSE" | jq .
    exit 1
else
    echo "✨ Successfully created fabric!"
    echo "$RESPONSE" | jq . # Pretty-print the new ID
fi