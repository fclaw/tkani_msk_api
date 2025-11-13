let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in

let
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
          --nix-shell-file=nix/stack-integration.nix \
        "
    '';
  };

in
pkgs.mkShell {
  buildInputs = [
    stack-wrapped
    # Other useful tools
    pkgs.git
    pkgs.docker
    pkgs.docker-compose
    pkgs.sqitchPg
    pkgs.postgresql
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
