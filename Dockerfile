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
COPY sql ./sql
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
# INSTALL MISSING SYSTEM LIBRARIES
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    netbase \
    ca-certificates \
    libelf1 \
    libdw1 \
    libnuma1 \
    curl \
    # wkhtmltopdf dependencies
    fontconfig \
    libxrender1 \
    libxext6 \
    libfontconfig1 \
    xfonts-75dpi \
    xfonts-base \
    wget \
    # Cyrillic Fonts
    fonts-dejavu-core \
    && rm -rf /var/lib/apt/lists/*

# 2. Download and install wkhtmltopdf for Bookworm
# The version 0.12.6.1-3 is specifically built for Debian 12 (Bookworm)
RUN wget https://github.com/wkhtmltopdf/packaging/releases/download/0.12.6.1-2/wkhtmltox_0.12.6.1-2.bullseye_amd64.deb \
    && dpkg -i wkhtmltopdf_0.12.6.1-2.bookworm_amd64.deb \
    || apt-get install -f -y \
    && rm wkhtmltopdf_0.12.6.1-2.bookworm_amd64.deb



# 2. DOWNLOAD THE CERTIFICATE (Self-contained)
# We download it directly to a system path. No local file needed.
RUN curl -k -o /app/cacert.pem https://curl.se/ca/cacert.pem
    
# 4. Copy Nix store, App binary, and Configs...
COPY --from=builder /deploy/nix/store /nix/store
COPY --from=builder /root/.local/bin/tkani-api-exe /app/server
COPY providers.yaml /app/
COPY templates /app/templates
COPY data /app/data
COPY config /app/config
COPY assets/templates/ /app/assets/templates

# 5. THE FIX: Update Entrypoint to look in System Folders too
RUN echo "#!/bin/sh" > /app/entrypoint.sh && \
    # We find Nix libs, BUT we also append :/usr/lib/x86_64-linux-gnu:/usr/lib
    # This tells the app: "Check Nix folders first, but if missing, check Debian folders!"
    echo "export LD_LIBRARY_PATH=\$(find /nix/store -name 'lib' -type d | paste -sd ':' -):/usr/lib/x86_64-linux-gnu:/usr/lib" >> /app/entrypoint.sh && \
    # Export SSL vars (keep this)
    echo "export SSL_CERT_FILE=/app/cacert.pem" >> /app/entrypoint.sh && \
    echo "export SYSTEM_CERTIFICATE_PATH=/app/cacert.pem" >> /app/entrypoint.sh && \
    echo "export NIX_SSL_CERT_FILE=/app/cacert.pem" >> /app/entrypoint.sh && \
    # Run
    echo "exec ./server" >> /app/entrypoint.sh && \
    chmod +x /app/entrypoint.sh

# or move shell.nix and nix-shell ./shell.nix -- stack exec tkani-api-exe -- no-metro

CMD ["/app/entrypoint.sh"]