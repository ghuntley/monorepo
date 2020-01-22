# Library for manipulating dates & times
{ pkgs, ... }:

let src = pkgs.third_party.fetchFromGitHub {
  owner = "dlowe-net";
  repo = "local-time";
  rev = "dc54f61415c76ee755a6f69d4154a3a282f2789f";
  sha256 = "1l9v07ghx7g9p2gp003fki4j8bsa1w2gbm40qc41i94mdzikc0ry";
};
in pkgs.nix.buildLisp.library {
  name = "local-time";
  deps = [ pkgs.third_party.lisp.cl-fad ];

  srcs = [
    "${src}/src/package.lisp"
    "${src}/src/local-time.lisp"
  ];
}
