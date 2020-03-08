{ pkgs, ... }:

pkgs.emacsPackages.trivialBuild rec {
  pname = "dottime";
  version = "1.0";
  src = ./dottime.el;
}
