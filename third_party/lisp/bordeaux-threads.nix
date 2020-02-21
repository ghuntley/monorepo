# This library is meant to make writing portable multi-threaded apps
# in Common Lisp simple.
{ depot, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/sionescu/bordeaux-threads.git";
  rev = "499b6d3f0ce635417d6096acf0a671d8bf3f6e5f";
};
in depot.nix.buildLisp.library {
  name = "bordeaux-threads";
  deps = [ depot.third_party.lisp.alexandria ];

  srcs = map (f: src + ("/src/" + f)) [
    "pkgdcl.lisp"
    "bordeaux-threads.lisp"
    "impl-sbcl.lisp"
    "default-implementations.lisp"
  ];
}
