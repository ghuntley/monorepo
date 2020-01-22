# A Common Lisp abstraction layer over platform dependent functionality.
{ pkgs, ... }:

let src = pkgs.third_party.fetchFromGitHub {
  owner = "svenvc";
  repo = "s-sysdeps";
  rev = "d28246b5dffef9e73a0e0e6cfbc4e878006fe34d";
  sha256 = "14b69b81yrxmjlvmm3lfxk04x5v7hqz4fql121334wh72czznfh9";
};
in pkgs.nix.buildLisp.library {
  name = "s-sysdeps";

  srcs = [
    "${src}/src/package.lisp"
    "${src}/src/sysdeps.lisp"
  ];
}
