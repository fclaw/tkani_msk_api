# ==========================================
# STAGE 1: BUILDER
# ==========================================
FROM nixos/nix:latest as builder

# 1. ENCODING
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

RUN echo "filter-syscalls = false" >> /etc/nix/nix.conf
WORKDIR /app

# 1. SETUP & DEPENDENCIES
COPY nix ./nix
COPY shell.nix ./
COPY stack.yaml package.yaml *.cabal ./

# Use a specific cache location for Docker layer caching
RUN nix-shell shell.nix --run "stack build --only-dependencies --system-ghc --no-nix"

# 2. BUILD BINARY
COPY src ./src
COPY app ./app
# COPY test ./test 

RUN nix-shell shell.nix --run "stack build --copy-bins --system-ghc --no-nix"

# 3. PREPARE MINIMAL RUNTIME CLOSURE (The Optimization Magic)
#    a. Instantiate the closure.nix file
#    b. Calculate all required paths (recursive) using nix-store -qR
#    c. Copy them to a 'deploy' directory
RUN mkdir -p /deploy/nix/store && \
    nix-instantiate nix/closure.nix --add-root ./runtime-root --indirect && \
    nix-store -r ./runtime-root --add-root ./runtime-result --indirect && \
    cp -r $(nix-store -qR ./runtime-result) /deploy/nix/store/

# ==========================================
# STAGE 2: RUNNER
# ==========================================
FROM debian:stable-slim

WORKDIR /app
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8


# 1. Install dependencies
# Added 'libelf1' here. This solves the missing shared object error.
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    netbase ca-certificates libelf1 \
    && rm -rf /var/lib/apt/lists/*

# 4. Copy Nix store, App binary, and Configs...
COPY --from=builder /deploy/nix/store /nix/store
COPY --from=builder /root/.local/bin/tkani-api-exe /app/server
COPY providers.yaml /app/
COPY templates /app/templates
COPY data /app/data

# 5. ENTRYPOINT
RUN echo "#!/bin/sh" > /app/entrypoint.sh && \
    echo "export LD_LIBRARY_PATH=\$(find /nix/store -name 'lib' -type d | paste -sd ':' -)" >> /app/entrypoint.sh && \
    echo "exec ./server" >> /app/entrypoint.sh && \
    chmod +x /app/entrypoint.sh

CMD ["/app/entrypoint.sh"]