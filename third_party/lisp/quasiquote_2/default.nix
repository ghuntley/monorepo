# Quasiquote more suitable for macros that define other macros
{ pkgs, ... }:

pkgs.nix.buildLisp.library {
  name = "quasiquote-2.0";

  deps = [
    pkgs.third_party.lisp.iterate
  ];

  srcs = [
    ./package.lisp
    ./quasiquote-2.0.lisp
    ./macros.lisp
    ./readers.lisp
  ];
}
