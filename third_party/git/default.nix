{ pkgs, ... }:

pkgs.third_party.originals.git.overrideAttrs(old: {
  doCheck = false;
  patches = [ ./dottime.patch ] ++ (if old ? patches then old.patches else []);
})
