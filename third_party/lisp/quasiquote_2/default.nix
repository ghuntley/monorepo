# Quasiquote more suitable for macros that define other macros
{ depot, ... }:

depot.nix.buildLisp.library {
  name = "quasiquote-2.0";

  deps = [
    depot.third_party.lisp.iterate
  ];

  srcs = [
    ./package.lisp
    ./quasiquote-2.0.lisp
    ./macros.lisp
    ./readers.lisp
  ];
}
