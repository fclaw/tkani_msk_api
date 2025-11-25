let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };

  # ENSURE THIS IS PRESENT: Reduced Locale Archive
  minimalLocales = pkgs.glibcLocales.override {
    allLocales = false;
    locales = ["en_US.UTF-8/UTF-8"];
  };
in
pkgs.symlinkJoin {
  name = "production-runtime";
  paths = [
    pkgs.postgresql.lib
    pkgs.zlib
    pkgs.gmp
    pkgs.libffi
    pkgs.iana-etc
    pkgs.cacert
    # --- ADD THIS (Crucial for DNS) ---
    pkgs.glibc
    # Force rebuild comment: Adding Libelf explicitly
    pkgs.libelf
    
    # REMOVED: pkgs.bash (Debian has this)
    
    minimalLocales
  ];
}