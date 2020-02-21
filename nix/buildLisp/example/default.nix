{ depot, ... }:

let
  inherit (depot.nix) buildLisp;

  # Example Lisp library.
  #
  # Currently the `name` attribute is only used for the derivation
  # itself, it has no practical implications.
  libExample = buildLisp.library {
    name = "lib-example";
    srcs = [
      ./lib.lisp
    ];
  };

# Example Lisp program.
#
# This builds & writes an executable for a program using the library
# above to disk.
#
# By default, buildLisp.program expects the entry point to be
# `$name:main`. This can be overridden by configuring the `main`
# attribute.
in buildLisp.program {
  name = "example";
  deps = [ libExample ];

  srcs = [
    ./main.lisp
  ];
}
