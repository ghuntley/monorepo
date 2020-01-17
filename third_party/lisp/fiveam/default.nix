# FiveAM is a Common Lisp testing framework.
#
# Imported from https://github.com/sionescu/fiveam.git

{ pkgs, ... }:

pkgs.nix.buildLisp.library {
  name = "fiveam";

  deps = with pkgs.third_party.lisp; [
    alexandria
    asdf-flv
    trivial-backtrace
  ];

  srcs = [
    ./src/package.lisp
    ./src/utils.lisp
    ./src/check.lisp
    ./src/fixture.lisp
    ./src/classes.lisp
    ./src/random.lisp
    ./src/test.lisp
    ./src/explain.lisp
    ./src/suite.lisp
    ./src/run.lisp
  ];
}
