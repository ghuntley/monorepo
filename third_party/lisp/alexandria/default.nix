# Alexandria is one of the foundational Common Lisp libraries that
# pretty much everything depends on:
#
# Imported from https://common-lisp.net/project/alexandria/
{ pkgs, ... }:

pkgs.nix.buildLisp.library {
  name = "alexandria";
  srcs = [
    ./package.lisp
    ./definitions.lisp
    ./binding.lisp
    ./strings.lisp
    ./conditions.lisp
    ./symbols.lisp
    ./macros.lisp
    ./functions.lisp
    ./io.lisp
    ./hash-tables.lisp
    ./control-flow.lisp
    ./lists.lisp
    ./types.lisp
    ./arrays.lisp
    ./sequences.lisp
    ./numbers.lisp
    ./features.lisp
  ];
}
