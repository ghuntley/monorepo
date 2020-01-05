# This file contains the configuration for my home desktop.

{ pkgs, ... }:

config: let
  inherit (pkgs) lib;

  nixpkgs = import pkgs.third_party.nixpkgsSrc {
    config.allowUnfree = true;
  };
in pkgs.lib.fix(self: {
  hardware = {
    pulseaudio.enable = true;
    cpu.intel.updateMicrocode = true;
  };

  boot = {
    cleanTmpDir = true;
    kernelModules = [ "kvm-intel" ];

    loader = {
      timeout = 3;
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = false;
    };

    initrd = {
      luks.devices.nugget-crypt.device = "/dev/disk/by-label/nugget-crypt";
      availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
      kernelModules = [ "dm-snapshot" ];
    };
  };

  nix = {
    nixPath = [
      "depot=/home/tazjin/depot"
      "nixpkgs=${pkgs.third_party.nixpkgsSrc}"
    ];
  };

  nixpkgs.pkgs = nixpkgs;

  networking = {
    hostName = "nugget";
    useDHCP = false;
    interfaces.eno1.useDHCP = true;
    interfaces.wlp7s0.useDHCP = true;

    # Don't use ISP's DNS servers:
    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
    ];

    # Open Chromecast-related ports & servedir
    firewall.allowedTCPPorts = [ 4242 5556 5558 ];
  };

  # Generate an immutable /etc/resolv.conf from the nameserver settings
  # above (otherwise DHCP overwrites it):
  environment.etc."resolv.conf" = with lib; with pkgs; {
    source = writeText "resolv.conf" ''
      ${concatStringsSep "\n" (map (ns: "nameserver ${ns}") self.networking.nameservers)}
      options edns0
    '';
  };

  time.timeZone = "Europe/London";

  environment.systemPackages =
    # programs from the depot
    (with pkgs; [
      (third_party.lieer {})
      ops.kontemplate
      third_party.git
      tools.emacs
    ]) ++

    # programs from nixpkgs
    (with nixpkgs; [
      age
      bat
      chromium
      curl
      direnv
      dnsutils
      exa
      fd
      gnupg
      go
      google-cloud-sdk
      htop
      imagemagick
      jq
      kubectl
      notmuch
      openssh
      openssl
      pass
      pavucontrol
      pinentry
      pinentry-emacs
      pwgen
      ripgrep
      rustup
      scrot
      spotify
      tokei
      tree
      vlc
      xclip
    ]);

    fileSystems = {
      "/".device = "/dev/disk/by-label/nugget-root";
      "/boot".device = "/dev/disk/by-label/EFI";
      "/home".device = "/dev/disk/by-label/nugget-home";
    };

    # Configure user account
    users.extraUsers.tazjin = {
      extraGroups = [ "wheel" "audio" ];
      isNormalUser = true;
      uid = 1000;
      shell = nixpkgs.fish;
    };

    security.sudo = {
      enable = true;
      extraConfig = "wheel ALL=(ALL:ALL) SETENV: ALL";
    };

    fonts = {
      fonts = with nixpkgs; [
        corefonts
        input-fonts
        noto-fonts-cjk
        noto-fonts-emoji
      ];
    };

    # Configure location (Vauxhall, London) for services that need it.
    location = {
      latitude = 51.4819109;
      longitude = -0.1252998;
    };

    programs.fish.enable = true;

    services.redshift.enable = true;
    services.openssh.enable = true;

    services.xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "caps:super";
      exportConfiguration = true;
      videoDrivers = [ "nvidia" ];

      displayManager = {
        # Give EXWM permission to control the session.
        sessionCommands = "${nixpkgs.xorg.xhost}/bin/xhost +SI:localuser:$USER";

        lightdm.enable = true;
        lightdm.greeters.gtk.clock-format = "%H·%M";
      };

      windowManager.session = pkgs.lib.singleton {
        name = "exwm";
        start = "${pkgs.tools.emacs}/bin/tazjins-emacs";
      };
    };

    # Do not restart the display manager automatically
    systemd.services.display-manager.restartIfChanged = lib.mkForce false;

    # ... and other nonsense.
    system.stateVersion = "19.09";
})
