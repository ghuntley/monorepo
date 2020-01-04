{ pkgs, ... }:

let
  inherit (pkgs) lib;
  inherit (builtins) foldl';

  systemFor = configs: (pkgs.third_party.nixos {
    configuration = lib.fix(config:
      foldl' lib.recursiveUpdate {} (map (c: c config) configs)
    );
  }).system;
in {
  # TODO(tazjin): rename 'pkgs' -> 'depot'?
  nuggetSystem = systemFor [ pkgs.ops.nixos.nugget ];
}
