{ pkgs, ... }:

pkgs.emacsPackagesNg.trivialBuild rec {
  pname = "dottime";
  version = "1.0";
  src = ./dottime.el;
}
