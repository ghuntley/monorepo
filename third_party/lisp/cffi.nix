# CFFI purports to be the Common Foreign Function Interface.
{ pkgs, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/cffi/cffi.git";
  rev = "5e838bf46d0089c43ebd3ea014a207c403e29c61";
};
in pkgs.nix.buildLisp.library {
  name = "cffi";
  deps = with pkgs.third_party.lisp; [
    alexandria
    asdf
    babel
    trivial-features
    uiop
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "cffi-sbcl.lisp"
    "package.lisp"
    "utils.lisp"
    "libraries.lisp"
    "early-types.lisp"
    "types.lisp"
    "enum.lisp"
    "strings.lisp"
    "structures.lisp"
    "functions.lisp"
    "foreign-vars.lisp"
    "features.lisp"
  ];
}
