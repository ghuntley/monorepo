# Imported from http://common-lisp.net/project/trivial-backtrace/trivial-backtrace.git
{ pkgs, ... }:

pkgs.nix.buildLisp.library {
  name = "trivial-backtrace";
  # deps = with pkgs.third_party.lisp; [ asdf ];

  srcs = [
    ./dev/packages.lisp
    ./dev/utilities.lisp
    ./dev/backtrace.lisp
    ./dev/map-backtrace.lisp
    ./dev/fallback.lisp
  ];
}
