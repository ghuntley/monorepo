# Imported from http://common-lisp.net/project/trivial-backtrace/trivial-backtrace.git
{ depot, ... }:

depot.nix.buildLisp.library {
  name = "trivial-backtrace";

  srcs = [
    ./dev/packages.lisp
    ./dev/utilities.lisp
    ./dev/backtrace.lisp
    ./dev/map-backtrace.lisp
    ./dev/fallback.lisp
  ];
}
