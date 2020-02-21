{ pkgs, ... }:

pkgs.emacsPackagesNg.trivialBuild rec {
  pname = "nix-util";
  version = "1.0";
  src = ./nix-util.el;
}
