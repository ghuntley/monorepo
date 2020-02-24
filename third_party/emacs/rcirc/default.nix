{ pkgs, ... }:

pkgs.emacsPackagesNg.trivialBuild rec {
  pname = "rcirc";
  version = "1";
  src = ./rcirc.el;
}
