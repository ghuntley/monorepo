# ASDF ships with SBCL. This package just exists to force it to load.
{ pkgs, ... }:

with pkgs;

nix.buildLisp.library {
  name = "asdf";
  srcs = lib.singleton (builtins.toFile "asdf.lisp" "(require 'asdf)");
}
