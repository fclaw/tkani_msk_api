let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz") {};
  haskellPackages = pkgs.haskell.packages.ghc967;
in
# This uses 'callCabal2nix' which is smart enough to read the package.yaml
# and figure out that it now needs servant-server and warp.
haskellPackages.callCabal2nix "simple-server" ./. {
  # We might need to add system-level build inputs here if servant/warp requires them
  # For now, it's likely not needed for this simple example.
}