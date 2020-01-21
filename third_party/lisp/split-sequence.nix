# split-sequence is a library for, well, splitting sequences apparently.
{ pkgs, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/sharplispers/split-sequence.git";
  rev = "41c0fc79a5a2871d16e5727969a8f699ef44d791";
};
in pkgs.nix.buildLisp.library {
  name = "split-sequence";
  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "vector.lisp"
    "list.lisp"
    "extended-sequence.lisp"
    "api.lisp"
    "documentation.lisp"
  ];
}
