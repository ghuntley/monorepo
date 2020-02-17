# Applies the changes from nixpkgs#88e4258df to the pounce derivation.
#
# TODO(tazjin): Remove this once the change has propagated to
# nixos-unstable.
{ pkgs, ... }:

with pkgs.third_party;

originals.pounce.overrideAttrs(old: rec {
  version = "1.1";
  src = fetchzip {
    url = "https://git.causal.agency/pounce/snapshot/pounce-${version}.tar.gz";
    sha256 = "07iyh6ikrlf7y57k462jcr00db6aijk9b2s7n7l7i49hk7kmm6wq";
  };

  patches = [];
})
