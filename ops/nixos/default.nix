{ depot, lib, ... }:

let
  inherit (builtins) foldl';

  systemFor = configs: (depot.third_party.nixos {
    configuration = lib.fix(config:
      foldl' lib.recursiveUpdate {} (map (c: c config) configs)
    );
  }).system;

  rebuilder = depot.third_party.writeShellScriptBin "rebuilder" ''
    set -ue
    if [[ $EUID -ne 0 ]]; then
      echo "Oh no! Only root is allowed to rebuild the system!" >&2
      exit 1
    fi

    case $HOSTNAME in
    nugget)
      echo "Rebuilding NixOS for //ops/nixos/nugget"
      system=$(nix-build -E '(import <depot> {}).ops.nixos.nuggetSystem' --no-out-link)
      ;;
    camden)
      echo "Rebuilding NixOS for //ops/nixos/camden"
      system=$(nix-build -E '(import <depot> {}).ops.nixos.camdenSystem' --no-out-link)
      ;;
    *)
      echo "$HOSTNAME is not a known NixOS host!" >&2
      exit 1
      ;;
    esac

    nix-env -p /nix/var/nix/profiles/system --set $system
    $system/bin/switch-to-configuration switch
  '';
in {
  inherit rebuilder;

  nuggetSystem = systemFor [ depot.ops.nixos.nugget ];
  camdenSystem = systemFor [ depot.ops.nixos.camden ];
}
