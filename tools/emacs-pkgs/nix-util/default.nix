{ pkgs, ... }:

pkgs.emacsPackages.trivialBuild rec {
  pname = "nix-util";
  version = "1.0";
  src = ./nix-util.el;
}
