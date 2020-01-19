# Override Guile to version 3.0.0
{ pkgs, ... }:

pkgs.third_party.originals.guile.overrideAttrs(old: rec {
  name = "guile-${version}";
  version = "3.0.0";

  src = pkgs.third_party.fetchurl {
    url = "mirror://gnu/guile/${name}.tar.xz";
    sha256 = "0x8ca6q1qdmk29lh12gj6ngvgn7kp79w42rxfgwrpxm9jmjqs4y9";
  };
})
