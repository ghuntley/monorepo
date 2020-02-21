{ depot, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/trivial-features/trivial-features.git";
  rev = "b78b2df5d75bdf8fdfc69f0deec0a187d9664b0b";
};
in depot.nix.buildLisp.library {
  name = "trivial-features";
  srcs = [
    (src + "/src/tf-sbcl.lisp")
  ];
}
