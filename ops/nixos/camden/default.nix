# This file configures camden.tazj.in, my homeserver.

{ pkgs, lib, ... }:

config: let
  nixpkgs = import pkgs.third_party.nixpkgsSrc {
    config.allowUnfree = true;
  };
in pkgs.lib.fix(self: {
  imports = [ ../modules/tailscale.nix ];

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

  nix = {
    maxJobs = lib.mkDefault 4;

    nixPath = [
      "depot=/home/tazjin/depot"
      "nixpkgs=${pkgs.third_party.nixpkgsSrc}"
    ];
  };
  nixpkgs.pkgs = nixpkgs;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  networking = {
    hostName = "camden";
    interfaces.enp1s0.useDHCP = true;
    firewall.allowedTCPPorts = [ 22 8080 80 443 ];
  };

  time.timeZone = "UTC";

  # System-wide application setup
  programs.fish.enable = true;
  environment.systemPackages =
    # programs from the depot
    (with pkgs; [
      third_party.git
      third_party.tailscale
    ]) ++

    # programs from nixpkgs
    (with nixpkgs; [
      curl emacs26-nox gnupg pass pciutils direnv
    ]);

  # Services setup
  services.openssh.enable = true;

  users.users.tazjin = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
    shell = nixpkgs.fish;
  };

  # Join Tailscale into home network
  services.tailscale = {
    enable = true;
    relayConf = "/etc/tailscale.conf";
    package = pkgs.third_party.tailscale;
    aclFile = pkgs.nix.tailscale [
      # Allow any traffic from myself
      {
        Action = "accept";
        Users = [ "mail@tazj.in" ];
        Ports = [ "*:*" ];
      }
    ];
  };

  # serve my website
  services.nginx = {
    enable = true;
    enableReload = true;

    # recommendedTlsSettings = true;
    # recommendedGzipSettings = true;
    # recommendedProxySettings = true;

    commonHttpConfig = ''
      log_format json_combined escape=json
      '{'
          '"time_local":"$time_local",'
          '"remote_addr":"$remote_addr",'
          '"remote_user":"$remote_user",'
          '"request":"$request",'
          '"status": "$status",'
          '"body_bytes_sent":"$body_bytes_sent",'
          '"request_time":"$request_time",'
          '"http_referrer":"$http_referer",'
          '"http_user_agent":"$http_user_agent"'
      '}';

      access_log /var/log/nginx_access.log json_combined;
    '';

    virtualHosts.homepage = {
      serverName = "camden.tazj.in"; # TODO(tazjin): change to actual host later
      default = true;
      enableACME = true;
      root = pkgs.web.homepage;
      addSSL = true;

      extraConfig = ''
        ${pkgs.web.blog.oldRedirects}

        location ~* \.(webp|woff2)$ {
          add_header Cache-Control "public, max-age=31536000";
        }

        location /blog/ {
          alias ${pkgs.web.blog.rendered}/;

          if ($request_uri ~ ^/(.*)\.html$) {
            return 302 /$1;
          }

          try_files $uri $uri.html $uri/ =404;
        }
      '';
    };
  };

  system.stateVersion = "19.09";
})
