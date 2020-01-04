{ pkgs, ... }:

let
  inherit (pkgs) third_party lib;
  configuration = rec {
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.cleanTmpDir = true;
    hardware.pulseaudio.enable = true;
    hardware.cpu.intel.updateMicrocode = true;
    time.timeZone = "Europe/London";

    networking = {
      # Don't use ISP's DNS servers:
      nameservers = [
        "8.8.8.8"
        "8.8.4.4"
      ];

      # Open Chromecast-related ports & servedir
      firewall.allowedTCPPorts = [ 3000 5556 5558 ];
    };

    # Generate an immutable /etc/resolv.conf from the nameserver settings
    # above (otherwise DHCP overwrites it):
    environment.etc."resolv.conf" = with lib; with pkgs; {
      source = writeText "resolv.conf" ''
        ${concatStringsSep "\n" (map (ns: "nameserver ${ns}") networking.nameservers)}
        options edns0
      '';
    };

    nixpkgs.config.allowUnfree = true;
  };

  # Desktop at home
  stallo = {
    networking.hostName = "stallo";
    services.xserver.videoDrivers = [ "nvidia" ];
    boot.initrd.luks.devices.stallo-luks.device = "/dev/disk/by-uuid/b484cf1e-a27b-4785-8bd6-fa85a004b073";

    fileSystems."/".device = "/dev/disk/by-label/stallo-root";
  };
in {
  stallo = third_party.nixos {
    configuration = lib.recursiveUpdate configuration stallo;
  };
}
