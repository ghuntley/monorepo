# Enables ANSI colors for printing.
{ pkgs, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/pnathan/cl-ansi-text.git";
  rev = "257a5f19a2dc92d22f8fd772c0a78923b99b36a8";
};
in pkgs.nix.buildLisp.library {
  name = "cl-ansi-text";
  deps = with pkgs.third_party.lisp; [
    alexandria
    cl-colors2
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "cl-ansi-text.lisp"
    "define-colors.lisp"
  ];
}
