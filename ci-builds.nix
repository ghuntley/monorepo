# This file defines the derivations that should be built by CI.
#
# The plan is still to implement recursive tree traversal
# automatically and detect all derivations that have `meta.enableCI =
# true`, but this is currently more effort than it would save me.

with (import ./default.nix {}); [
  fun.amsterdump
  fun.gemma
  fun.quinistry
  fun.watchblob
  fun.wcl
  lisp.dns
  nix.buildLisp.example
  nix.yants.tests
  ops."posix_mq.rs"
  ops.besadii
  ops.journaldriver
  ops.kms_pass
  ops.kontemplate
  ops.mq_cli
  ops.nixos.camdenSystem
  ops.nixos.nuggetSystem
  third_party.cgit
  third_party.git
  third_party.lisp # will build all third-party libraries
  tools.cheddar
  tools.emacs
  web.blog
  web.cgit-taz
]
