# SB-ROTATE-BYTE is an SBCL component. This package just forces it to
# be loaded.
{ pkgs, ... }:

with pkgs;

nix.buildLisp.library {
  name = "sb-rotate-byte";
  srcs = lib.singleton (builtins.toFile "sb-rotate-byte.lisp" "(require 'sb-rotate-byte)");
}
