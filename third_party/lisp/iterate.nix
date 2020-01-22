# iterate is an iteration construct for Common Lisp, similar to the
# LOOP macro.
{ pkgs, ... }:

let src = builtins.fetchGit {
  url = "https://gitlab.common-lisp.net/iterate/iterate.git";
  rev = "a1c47b2b74f6c96149d717be90c07a1b273ced69";
};
in pkgs.nix.buildLisp.library {
  name = "iterate";
  srcs = [
    "${src}/package.lisp"
    "${src}/iterate.lisp"
  ];
}
