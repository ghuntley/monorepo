# SB-BSD-SOCKETS is an SBCL component. This package just forces it to
# be loaded.
{ pkgs, ... }:

with pkgs;

nix.buildLisp.library {
  name = "sb-bsd-sockets";
  srcs = lib.singleton (builtins.toFile "sb-bsd-sockets.lisp" "(require 'sb-bsd-sockets)");
}
