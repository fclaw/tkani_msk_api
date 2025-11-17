#!/bin/bash

# ==============================================================================
#  Start Script for the Tkani API Server
# ==============================================================================
# This script sets all required environment variables and then executes the
# Haskell application using 'stack exec'.
#
# USAGE: ./start.sh
#
# NOTE: Make sure to fill in the placeholder values below.
# For production, it's recommended to load these from a secure vault or a
# .env file that is NOT committed to Git.
# ==============================================================================

# --- SDEK API Credentials ---
# test account: wqGwiQx0gg8mLtiEKsUinjVSICCjtTEP
# test secret: RmAmgvSgSl1yirlz9QupbzOJVqhCxcP5
# account: oq9uzMnwlLs7XEBdIeg6nqLRVeK1UFm1
# secret: O5yBUbuoShnq5IVp975A4ZcjN0iJv7DK
export SDEK_CLIENT_ID="wqGwiQx0gg8mLtiEKsUinjVSICCjtTEP"
export SDEK_CLIENT_SECRET="RmAmgvSgSl1yirlz9QupbzOJVqhCxcP5"
export SDEK_URL="api.edu.cdek.ru" # Use 'api.cdek.ru' for production, 'api.edu.cdek.ru' for testing

# --- Telegram Logger Credentials ---
export TELEGRAM_BOT_TOKEN="8527850689:AAGhw8CZjrllGu6gkW7NZRv-ZC5cY_KAlxg"
export TELEGRAM_CHAT_ID="-1003307820920"

# --- Internal Telegram Orders Channel Credentials ---
export ORDER_BOT_TOKEN="8370864293:AAG1gO8yJoEVmNww3KljFDnsKdaXNuGcI4A"
export ORDER_CHAT_ID="-1003483940388"

# --- Yandex Maps API Key ---
export YANDEX_API_KEY="b1b5ebb7-b126-477e-b2bc-1510519f0eb5"

# --- Database Connection String (Example for PostgreSQL) ---
# It's better to read this from config or another env var, but you can set it here.
# export DB_CONNECTION_STRING="host=localhost port=5432 user=myuser dbname=mydb password=mypass"

# --- Announce the start ---
echo "================================================="
echo " INFO: Setting environment variables for Tkani API"
echo "================================================="
echo "SDEK_URL: $SDEK_URL"
echo "Running application with stack..."
echo ""

# --- Execute the Application ---
# 'stack exec' runs the compiled executable for the 'tkani-api-exe' component
# defined in your package.yaml or .cabal file.
# The executable will automatically inherit all the environment variables
# we just exported.
stack exec tkani-api-exe