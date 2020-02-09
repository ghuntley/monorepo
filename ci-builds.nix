# This file defines the derivations that should be built by CI.
#
# The plan is still to implement recursive tree traversal
# automatically and detect all derivations that have `meta.enableCI =
# true`, but this is currently more effort than it would save me.

let
  pkgs = import ./default.nix {};
in with pkgs; [
  fun.gemma
  nix.yants.tests
  ops."posix_mq.rs"
  ops.journaldriver
  ops.kms_pass
  ops.kontemplate
  ops.mq_cli
  ops.sync-gcsr
  tools.cheddar
  tools.emacs
  web.blog
  web.cgit-taz
  lisp.dns
  third_party.cgit
  third_party.git
  third_party.guile
  third_party.lisp # will build all third-party libraries
]
