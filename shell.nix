let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in

let
  # Explicitly select the GHC version we want from Nixpkgs.
  # If Nixpkgs has 9.10.3 specifically, this grabs it. 
  # If it only has 9.10.1, 'ghc910' usually points to that.
  myHaskellPkgs = pkgs.haskell.packages.ghc9102;
  # Wrap Stack to configure Nix integration and target the correct Stack-Nix file
  #
  # - nix: Enable Nix support
  # - no-nix-pure: Pass environment variables, like `NIX_PATH`
  # - nix-shell-file: Specify the Nix file to use (otherwise it uses `shell.nix` by default)
  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --no-nix-pure \
          --nix-shell-file=shell.nix \
        "
    '';
  };

in
pkgs.mkShell {
    # NATIVE inputs = Build tools (must exist during compilation)
  nativeBuildInputs = [
    pkgs.pkg-config  # <--- CRITICAL: Finds libraries for postgres/zlib
  ];
   # BUILD inputs = Libraries your app links against
  buildInputs = [
    stack-wrapped
    # We add the specific GHC here so `stack --nix` finds the right compiler
    myHaskellPkgs.ghc
    # Other useful tools
    pkgs.git
    pkgs.docker
    pkgs.docker-compose
    pkgs.sqitchPg

    # --- System Dependencies (The ones you identified) ---
    pkgs.postgresql
    pkgs.zlib
    pkgs.zlib.dev    # <--- CRITICAL: Headers for zlib
    pkgs.gmp
    pkgs.xz          # often needed for compression
  ];

  # Configure the Nix path to our own `pkgs`, to ensure Stack-with-Nix uses the correct one rather than the global <nixpkgs> when looking for the right `ghc` argument to pass in `nix/stack-integration.nix`
  # See https://nixos.org/nixos/nix-pills/nix-search-paths.html for more information
  NIX_PATH = "nixpkgs=" + pkgs.path;

  # This shellHook is not strictly necessary but is a good practice.
  # It warns the user if their system's GHC is different from Stack's.
  shellHook = ''
    echo "✅ Entered Nix-based Stack development shell."
    echo "   Using stack provided by Nix. It will manage its own GHC."
    echo "   Available commands: stack, ghcid, hpack, docker, ..."
    echo "   Using GHC: $(ghc --version)"

    # Ensure the linker finds the libraries
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.postgresql pkgs.zlib pkgs.gmp ]}:$LD_LIBRARY_PATH
    
    # Docker socket setup for macOS
    DOCKER_SOCKET_PATH="/var/run/docker.sock"
    if [ -S "$DOCKER_SOCKET_PATH" ]; then
      if [ -L "$DOCKER_SOCKET_PATH" ]; then
        DOCKER_SOCKET_PATH=$(readlink $DOCKER_SOCKET_PATH)
      fi
      export DOCKER_HOST="unix://$DOCKER_SOCKET_PATH"
      echo "   🐳 Docker host configured to use: $DOCKER_HOST"
    fi
  '';
}