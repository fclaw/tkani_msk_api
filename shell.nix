# This is now the single source of truth for nixpkgs.
# We no longer take pkgs as a function argument.
let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz";

  # --- THE FIX: Create an overlay to override dependencies ---
  # An overlay is a set of modifications applied to nixpkgs.
  overlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper: {
        # This tells Nix: for the 'hasql' package,
        # ignore its default dependencies and build it with this specific version of 'transformers'.
        hasql = hsuper.callCabal2nix "hasql" hsuper.hasql_1_9_3_2 {
          transformers = hsuper.transformers_0_6_2_0;
        };
      };
    };
  };

  # Import nixpkgs and apply our overlay
  pkgs = import nixpkgs { overlays = [ overlay ]; };
  
  # Select the Haskell package set for a specific GHC version from our modified pkgs
  # (Let's use a known recent version like ghc963 for unstable)
  haskellPackages = pkgs.haskell.packages.ghc967;

  # Define our Haskell dependencies here, including the missing one.
  ghcWithPackages = haskellPackages.ghc.withPackages (ps: with ps; [
    # Dependencies from your package.yaml
    aeson
    bytestring
    hasql
    hasql-pool
    hasql-transaction
    katip
    mtl
    servant-server
    servant-swagger
    servant-swagger-ui
    text
    transformers
    wai
    warp

    # --- THE FIX ---
    # Explicitly add the 'unix' package that was missing.
    unix
  ]);

in
pkgs.mkShell {
  # These are the packages made available in the shell environment.
  buildInputs = [
    ghcWithPackages # This provides ghc, cabal, and all our haskell deps

    # --- System Libraries and Tools ---
    
    # Correct package name for Sqitch with PostgreSQL support
    pkgs.sqitchPg

    # The PostgreSQL C library itself (needed by hasql and sqitchPg)
    pkgs.postgresql

    # Other common dependencies for Haskell builds
    pkgs.zlib
    pkgs.pkg-config
    pkgs.git

    # Docker client (for building images)
    pkgs.docker
  ];

  # The shellHook is executed when you enter the nix-shell
  shellHook = ''
    echo "Entering Nix-based Haskell development shell..."
    
    # Docker socket setup
    DOCKER_SOCKET_PATH="/var/run/docker.sock"
    if [ -L "$DOCKER_SOCKET_PATH" ]; then
        DOCKER_SOCKET_PATH=$(readlink $DOCKER_SOCKET_PATH)
    fi
    export DOCKER_HOST="unix://$DOCKER_SOCKET_PATH"
    echo "Docker host configured to use: $DOCKER_HOST"
  '';
}