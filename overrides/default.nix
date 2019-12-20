# This file is used to move things from nested attribute sets to the
# top-level.
{ pkgs, ... }:

{
  buildGo = pkgs.nix.buildGo;

  # These packages must be exposed at the top-level for compatibility
  # with Nixery.
  inherit (pkgs.third_party)
    bashInteractive
    cacert
    coreutils
    iana-etc
    jq
    moreutils
    nano
    openssl
    runCommand
    symlinkJoin
    writeText;

  # These packages must be exposed for compatibility with buildGo.
  #
  # Despite buildGo being tracked in this tree, I want it to be possible
  # for external users to import it with the default nixpkgs layout.
  inherit (pkgs.third_party) go ripgrep;
}
