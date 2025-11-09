# --- Stage 1: The Nix-based Builder ---
# Start from the official NixOS image, which has Nix installed.
FROM nixos/nix:latest as builder

# Set the working directory
WORKDIR /app

# Copy all your project files into the container.
# This includes .cabal, package.yaml, shell.nix, and your src/ and app/ dirs.
COPY . .

# --- The Magic Command ---
# Use 'nix-build' to produce the executable.
# This command tells Nix to build the default derivation in the current directory.
# We will create a default.nix file to define what that is.
# The result will be a symlink at `./result/bin/tkani-backend-exe`
RUN nix-build

# --- Stage 2: The Final Production Image ---
# Start from a minimal base image.
FROM debian:bookworm-slim

# The 'postgresql-client' provides 'libpq.so.5' and 'zlib1g' provides zlib.
# These are the runtime C libraries that our statically-linked executable still needs.
RUN apt-get update && apt-get install -y --no-install-recommends postgresql-client zlib1g && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy ONLY the compiled executable from the builder stage.
# The path inside the nix store is complex, but the `result` symlink gives us a stable path.
COPY --from=builder /app/result/bin/tkani-backend-exe .


# 1. Declare a build-time argument with a default value.
ARG APP_PORT=8080

# ... (Copy files, etc.) ...

# 2. Use the ARG in the EXPOSE instruction.
EXPOSE $APP_PORT

# 3. Your Haskell/C++ app should be configured to listen on this port.
#    You can pass this ARG as an ENV variable to the final container.
ENV PORT=$APP_PORT


# The command to run the application
CMD ["./tkani-backend-exe"]