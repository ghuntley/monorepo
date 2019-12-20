# This file defines the derivations that should be built by CI.
#
# The plan is still to implement recursive tree traversal
# automatically and detect all derivations that have `meta.enableCI =
# true`, but this is currently more effort than it would save me.

let
  pkgs = import ./default.nix {};
in with pkgs; [
  ops.journaldriver
  ops.kms_pass
  ops.sync-gcsr
  tools.blog_cli
  tools.emacs
  web.cgit-taz

  # web.tazblog #  TODO(tazjin): Happstack build failure in nixos-unstable
]
