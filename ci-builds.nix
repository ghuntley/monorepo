# This file defines the derivations that should be built by CI.
#
# The plan is still to implement recursive tree traversal
# automatically and detect all derivations that have `meta.enableCI =
# true`, but this is currently more effort than it would save me.

let
  pkgs = import ./default.nix {};
in with pkgs; [
  nix.yants.tests
  ops.journaldriver
  ops.kms_pass
  ops.kontemplate
  ops.sync-gcsr
  tools.blog_cli
  tools.cheddar
  tools.emacs
  web.cgit-taz
  third_party.cgit
  third_party.git
  third_party.guile
  third_party.lisp # will build all third-party libraries
  # web.tazblog #  TODO(tazjin): Happstack build failure in nixos-unstable
]
