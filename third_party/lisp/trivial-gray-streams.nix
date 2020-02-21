# Portability library for CL gray streams.
{ depot, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/trivial-gray-streams/trivial-gray-streams.git";
  rev = "ebd59b1afed03b9dc8544320f8f432fdf92ab010";
};
in depot.nix.buildLisp.library {
  name = "trivial-gray-streams";
  srcs = [
    (src + "/package.lisp")
    (src + "/streams.lisp")
  ];
}


