# trivial-garbage provides a portable API to finalizers, weak
# hash-tables and weak pointers
{ pkgs, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/trivial-garbage/trivial-garbage.git";
  rev = "dbc8e35acb0176b9a14fdc1027f5ebea93435a84";
};
in pkgs.nix.buildLisp.library {
  name = "trivial-garbage";
  srcs = [ (src + "/trivial-garbage.lisp") ];
}
