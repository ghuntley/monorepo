# SB-POSIX is an SBCL component. This package just forces it to be
# loaded.
{ pkgs, ... }:

with pkgs;

nix.buildLisp.library {
  name = "sb-posix";
  srcs = lib.singleton (builtins.toFile "sb-posix.lisp" "(require 'sb-posix)");
}
