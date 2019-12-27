{ pkgs, ... }:

pkgs.third_party.originals.notmuch.overrideAttrs(old: {
  patches = [ ./dottime.patch ] ++ (if old ? patches then old.patches else []);
})
