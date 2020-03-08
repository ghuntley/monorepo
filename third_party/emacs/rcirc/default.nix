{ pkgs, ... }:

pkgs.emacsPackages.trivialBuild rec {
  pname = "rcirc";
  version = "1";
  src = ./rcirc.el;
}
