# This file configures camden.tazj.in, my homeserver.

{ pkgs, lib, ... }:

config: let
  nixpkgs = import pkgs.third_party.nixpkgsSrc {
    config.allowUnfree = true;
  };
in pkgs.lib.fix(self: {
  # camden is intended to boot unattended, despite having an encrypted
  # root partition.
  #
  # The below configuration uses an externally connected USB drive
  # that contains a LUKS key file to unlock the disk automatically at
  # boot.
  #
  # TODO(tazjin): Configure LUKS unlocking via SSH instead.
  boot = {
    initrd = {
      availableKernelModules = [
        "ahci" "xhci_pci" "usbhid" "usb_storage" "sd_mod" "sdhci_pci"
        "rtsx_usb_sdmmc" "r8169"
      ];

      kernelModules = [ "dm-snapshot" ];

      luks.devices.camden-crypt = {
        fallbackToPassword = true;
        device = "/dev/disk/by-label/camden-crypt";
        keyFile = "/dev/sdb";
        keyFileSize = 4096;
      };
    };

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    cleanTmpDir = true;
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/camden-root";
      fsType = "ext4";
    };

    "/home" = {
      device = "/dev/disk/by-label/camden-home";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-label/BOOT";
      fsType = "vfat";
    };
  };


  # TODO(tazjin): audit these (from generated hardware-config)
  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  networking = {
    hostName = "camden";
    interfaces.enp1s0.useDHCP = true;
    firewall.allowedTCPPorts = [ 22 8080 80 443 ];
  };

  time.timeZone = "UTC";

  # System-wide application setup
  programs.fish.enable = true;
  environment.systemPackages = with nixpkgs; [
    curl emacs26-nox git gnupg pass pciutils
  ];

  # Services setup
  services.openssh.enable = true;

  users.users.tazjin = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
    shell = nixpkgs.fish;
  };

  system.stateVersion = "19.09";
})
