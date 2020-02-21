# Portable URI library
{ depot, ... }:

let src = builtins.fetchGit {
  url = "http://git.kpe.io/puri.git";
  rev = "ef5afb9e5286c8e952d4344f019c1a636a717b97";
};
in depot.nix.buildLisp.library {
  name = "puri";
  srcs = [
    (src + "/src.lisp")
  ];
}


