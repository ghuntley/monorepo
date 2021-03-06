# This file sets up the top-level package set by traversing the package tree
# (see read-tree.nix for details) and constructing a matching attribute set
# tree.
#
# This makes packages accessible via the Nixery instance that is configured to
# use this repository as its nixpkgs source.

{ ... }@args:

with builtins;

let
  # This definition of fix is identical to <nixpkgs>.lib.fix, but the global
  # package set is not available here.
  fix = f: let x = f x; in x;

  # Global configuration that all packages are called with.
  config = depot: {
    inherit depot;

    # Pass third_party as 'pkgs' (for compatibility with external
    # imports for certain subdirectories)
    pkgs = depot.third_party;

    kms = {
      project = "tazjins-infrastructure";
      region = "europe-north1";
      keyring = "tazjins-keys";
      key = "kontemplate-key";
    };
  };

  readTree' = import ./nix/readTree {};

  localPkgs = readTree: {
    fun           = readTree ./fun;
    nix           = readTree ./nix;
    ops           = readTree ./ops;
    lisp          = readTree ./lisp;
    presentations = readTree ./presentations;
    third_party   = readTree ./third_party;
    tools         = readTree ./tools;
    web           = readTree ./web;
  };
in fix(self: {
  config = config self;

  # Elevate 'lib' from nixpkgs
  lib = import (self.third_party.nixpkgsSrc + "/lib");

  # Expose readTree for downstream repo consumers.
  readTree = {
    __functor = x: (readTree' x.config);
    config = self.config;
  };
}

# Add local packages as structured by readTree
// (localPkgs (readTree' (self.config // { inherit (self) lib; })))

# Load overrides into the top-level.
#
# This can be used to move things from third_party into the top-level, too (such
# as `lib`).
// (readTree' { depot = self; pkgs = self.third_party; }) ./overrides
)
