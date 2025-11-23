# Use the official Nix image
FROM nixos/nix:latest

# 1. FIX THE LOCALE/ENCODING HERE
# We manually set these env vars. NixOS image has C.UTF-8 support built-in
# which doesn't require downloading extra locale packages.
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

# Configure Nix
RUN echo "filter-syscalls = false" >> /etc/nix/nix.conf

WORKDIR /app

# --- LAYER 1: Dependencies ---
COPY nix ./nix
COPY shell.nix ./
COPY stack.yaml package.yaml ./

# Force cache download
RUN nix-shell shell.nix --run "echo '✅ Nix environment cached'"

# --- LAYER 2: Application Build ---
COPY src ./src
COPY app ./app
# ... copy other dirs ...

# Build
RUN nix-shell shell.nix --run "stack build --copy-bins"

# ... rest of file ...