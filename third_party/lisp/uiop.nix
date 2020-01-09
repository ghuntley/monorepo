# UIOP ships with SBCL (due to ASDF). This package just exists to
# force it to load.
{ pkgs, ... }:

with pkgs;

nix.buildLisp.library {
  name = "uiop";
  srcs = lib.singleton (builtins.toFile "uiop.lisp" "(require 'uiop)");
}
