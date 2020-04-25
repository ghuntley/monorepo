# This file configures nugget, my home desktop machine.
{ depot, lib, ... }:

config: let
  nixpkgs = import depot.third_party.nixpkgsSrc {
    config.allowUnfree = true;
  };

  lieer = (depot.third_party.lieer {});

  # google-c-style is installed only on nugget because other
  # machines get it from, eh, elsewhere.
  nuggetEmacs = (depot.tools.emacs.overrideEmacs(epkgs: epkgs ++ [
    depot.third_party.emacsPackages.google-c-style
  ]));
in depot.lib.fix(self: {
  imports = [
    ../modules/v4l2loopback.nix
  ];

  hardware = {
    pulseaudio.enable = true;
    cpu.intel.updateMicrocode = true;
    u2f.enable = true;
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
      "nixpkgs=${depot.third_party.nixpkgsSrc}"
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
    firewall.enable = false;
    firewall.allowedTCPPorts = [ 4242 5556 5558 ];

    # Connect to the WiFi to let the Chromecast work.
    wireless.enable = true;
    wireless.networks = {
      "How do I computer?" = {
        psk = "washyourface";
      };
    };
  };

  # Generate an immutable /etc/resolv.conf from the nameserver settings
  # above (otherwise DHCP overwrites it):
  environment.etc."resolv.conf" = with lib; {
    source = depot.third_party.writeText "resolv.conf" ''
      ${concatStringsSep "\n" (map (ns: "nameserver ${ns}") self.networking.nameservers)}
      options edns0
    '';
  };

  time.timeZone = "Europe/London";

  environment.systemPackages =
    # programs from the depot
    (with depot; [
      lieer
      nuggetEmacs
      ops.kontemplate
      third_party.chromiumVaapi
      third_party.ffmpeg
      third_party.git
    ]) ++

    # programs from nixpkgs
    (with nixpkgs; [
      age
      bat
      cachix
      clang-manpages
      clang-tools
      clang_9
      curl
      direnv
      dnsutils
      exa
      fd
      gnupg
      go
      google-chrome
      google-cloud-sdk
      guile
      htop
      i3lock
      imagemagick
      jq
      keybase-gui
      kubectl
      miller
      msmtp
      nix-prefetch-github
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
      sbcl
      scrot
      spotify
      steam
      tokei
      tree
      unzip
      vlc
      xclip
      yubico-piv-tool
      yubikey-personalization
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
        dejavu_fonts
        jetbrains-mono
        noto-fonts-cjk
        noto-fonts-emoji
      ];

      fontconfig = {
        hinting.enable = true;
        subpixel.lcdfilter = "light";

        defaultFonts = {
          monospace = [ "JetBrains Mono" ];
        };
      };
    };

    # Configure location (Vauxhall, London) for services that need it.
    location = {
      latitude = 51.4819109;
      longitude = -0.1252998;
    };

    programs.fish.enable = true;

    services.redshift.enable = true;
    services.openssh.enable = true;
    services.keybase.enable = true;

    # Required for Yubikey usage as smartcard
    services.pcscd.enable = true;
    services.udev.packages = [
      nixpkgs.yubikey-personalization
    ];

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

      windowManager.session = lib.singleton {
        name = "exwm";
        start = "${nuggetEmacs}/bin/tazjins-emacs";
      };
    };

    # Do not restart the display manager automatically
    systemd.services.display-manager.restartIfChanged = lib.mkForce false;

    # Configure email setup
    systemd.user.services.lieer-tazjin = {
      description = "Synchronise mail@tazj.in via lieer";
      script = "${lieer}/bin/gmi sync";

      serviceConfig = {
        WorkingDirectory = "%h/mail/account.tazjin";
        Type = "oneshot";
      };
    };

    systemd.user.timers.lieer-tazjin = {
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnActiveSec = "1";
        OnUnitActiveSec = "180";
      };
    };

    # Use Tailscale \o/
    services.tailscale.enable = true;

    # ... and other nonsense.
    system.stateVersion = "19.09";
})
