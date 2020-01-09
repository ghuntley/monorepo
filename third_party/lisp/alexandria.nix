# Alexandria is one of the foundational Common Lisp libraries that
# pretty much everything depends on:
#
# https://common-lisp.net/project/alexandria/
{ pkgs, ... }:

let src = builtins.fetchGit {
  url = "https://gitlab.common-lisp.net/alexandria/alexandria";
  rev = "1224346a48770dc07a68ef3e612b9ac3d611eb07";
};
in pkgs.nix.buildLisp.library {
  name = "alexandria";
  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "definitions.lisp"
    "binding.lisp"
    "strings.lisp"
    "conditions.lisp"
    "symbols.lisp"
    "macros.lisp"
    "functions.lisp"
    "io.lisp"
    "hash-tables.lisp"
    "control-flow.lisp"
    "lists.lisp"
    "types.lisp"
    "arrays.lisp"
    "sequences.lisp"
    "numbers.lisp"
    "features.lisp"
  ];
}
