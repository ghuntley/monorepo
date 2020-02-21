# cl-ppcre is a Common Lisp regular expression library.
{ depot, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/edicl/cl-ppcre";
  rev = "1ca0cd9ca0d161acd49c463d6cb5fff897596e2f";
};
in depot.nix.buildLisp.library {
  name = "cl-ppcre";

  srcs = map (f: src + ("/" + f)) [
    "packages.lisp"
    "specials.lisp"
    "util.lisp"
    "errors.lisp"
    "charset.lisp"
    "charmap.lisp"
    "chartest.lisp"
    "lexer.lisp"
    "parser.lisp"
    "regex-class.lisp"
    "regex-class-util.lisp"
    "convert.lisp"
    "optimize.lisp"
    "closures.lisp"
    "repetition-closures.lisp"
    "scanner.lisp"
    "api.lisp"
  ];
}
