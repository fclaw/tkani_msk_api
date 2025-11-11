# --- Stage 1: The Stack-based Builder ---
# Start from an official image from the 'fpco' (Formal Methods Company)
# that has Stack and common Haskell dependencies pre-installed.
# Using a specific LTS version ensures reproducibility.
FROM fpco/stack-build:lts-24.19 as builder

# Set the working directory
WORKDIR /app

# --- Dependency Caching Optimization ---
# Copy the stack configuration files first.
COPY stack.yaml package.yaml ./

# This command downloads and builds all Haskell dependencies defined in package.yaml.
# Docker will cache this layer and only re-run it if your dependency list changes.
RUN stack build --only-dependencies

# Copy the rest of your application source code (src/, app/, etc.)
COPY . .

# --- The Build Command ---
# Now, build your actual project. This will be fast because dependencies are cached.
# 'stack install' compiles the code and copies the final executable to a known location.
RUN stack install --local-bin-path /usr/local/bin


# --- Stage 2: The Final Production Image ---
# Start from a minimal base image. 'debian:slim' is a good choice.
FROM debian:bookworm-slim

# Install the RUNTIME C libraries that our "static" Haskell executable might still need.
# Even statically linked binaries often need a few core libraries like libpq and zlib.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev \
    zlib1g \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy ONLY the compiled executable from the builder stage.
COPY --from=builder /usr/local/bin/tkani-api-exe .

# Expose the port your Warp server listens on.
EXPOSE 8080

# The command to run the application when the container starts.
CMD ["./tkani-api-exe"]