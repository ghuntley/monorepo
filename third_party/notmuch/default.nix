{ pkgs, ... }:

pkgs.originals.notmuch.overrideAttrs(old: {
  doCheck = false;
  patches = [ ./dottime.patch ] ++ (if old ? patches then old.patches else []);
})
