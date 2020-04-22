# This file configures camden.tazj.in, my homeserver.
{ depot, lib, ... }:

config: let
  nixpkgs = import depot.third_party.nixpkgsSrc {
    config.allowUnfree = true;
  };
in lib.fix(self: {
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
      "nixpkgs=${depot.third_party.nixpkgsSrc}"
    ];

    trustedUsers = [ "root" "tazjin" ];

    binaryCaches = [
      "https://tazjin.cachix.org"
    ];

    binaryCachePublicKeys = [
      "tazjin.cachix.org-1:IZkgLeqfOr1kAZjypItHMg1NoBjm4zX9Zzep8oRSh7U="
    ];
  };
  nixpkgs.pkgs = nixpkgs;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  networking = {
    hostName = "camden";
    interfaces.enp1s0.useDHCP = true;
    interfaces.enp1s0.ipv6.addresses = [
      {
        address = "2a01:4b00:821a:ce02::5";
        prefixLength = 64;
      }
    ];

    firewall.allowedTCPPorts = [ 22 8080 80 443 1935 ];
  };

  time.timeZone = "UTC";

  # System-wide application setup
  programs.fish.enable = true;
  programs.mosh.enable = true;

  environment.systemPackages =
    # programs from the depot
    (with depot; [
      third_party.git
      third_party.honk
      third_party.pounce
    ]) ++

    # programs from nixpkgs
    (with nixpkgs; [
      bat
      curl
      direnv
      emacs26-nox
      gnupg
      htop
      jq
      pass
      pciutils
      ripgrep
    ]);

  users = {
    # Set up my own user for logging in and doing things ...
    users.tazjin = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "git" "wheel" ];
      shell = nixpkgs.fish;
    };

    # Set up a user & group for general git shenanigans
    groups.git = {};
    users.git = {
      group = "git";
      isNormalUser = false;
    };
  };

  # Services setup
  services.openssh.enable = true;
  services.haveged.enable = true;

  # Join Tailscale into home network
  services.tailscale.enable = true;

  # Run cgit for the depot. The onion here is nginx(thttpd(cgit)).
  systemd.services.cgit = {
    wantedBy = [ "multi-user.target" ];
    script = "${depot.web.cgit-taz}/bin/cgit-launch";

    serviceConfig = {
      Restart = "on-failure";
      User = "git";
      Group = "git";
    };
  };

  # Run honk as the ActivityPub server, using all the fancy systemd
  # magic.
  systemd.services.honk = {
    wantedBy = [ "multi-user.target" ];
    script = lib.concatStringsSep " " [
      "${depot.third_party.honk}/bin/honk"
      "-datadir /var/lib/honk"
      "-viewdir ${depot.third_party.honk.src}"
    ];

    serviceConfig = {
      Restart = "always";
      DynamicUser = true;
      StateDirectory = "honk";
      WorkingDirectory = "/var/lib/honk";
    };
  };

  # NixOS 20.03 broke nginx and I can't be bothered to debug it
  # anymore, all solution attempts have failed, so here's a
  # brute-force fix.
  systemd.services.fix-nginx = {
    script = "${nixpkgs.coreutils}/bin/chown -R nginx: /var/spool/nginx";

    serviceConfig = {
      User = "root";
      Type = "oneshot";
    };
  };

  systemd.timers.fix-nginx = {
    wantedBy = [ "multi-user.target" ];
    timerConfig = {
      OnCalendar = "minutely";
    };
  };

  # Provision a TLS certificate outside of nginx to avoid
  # nixpkgs#38144
  security.acme = {
    acceptTerms = true;
    email = "mail@tazj.in";

    certs."tazj.in" = {
      user = "nginx";
      group = "nginx";
      webroot = "/var/lib/acme/acme-challenge";
      extraDomains = {
        "git.tazj.in" = null;
        "www.tazj.in" = null;

        # Local domains (for this machine only)
        "camden.tazj.in" = null;
        "git.camden.tazj.in" = null;
      };
      postRun = "systemctl reload nginx";
    };

    certs."tvl.fyi" = {
      user = "nginx";
      group = "nginx";
      webroot = "/var/lib/acme/acme-challenge";
      postRun = "systemctl reload nginx";
    };
  };

  # Forward logs to Google Cloud Platform
  services.journaldriver = {
    enable                 = true;
    logStream              = "home";
    googleCloudProject     = "tazjins-infrastructure";
    applicationCredentials = "/etc/gcp/key.json";
  };

  # serve my website
  services.nginx = {
    enable = true;
    enableReload = true;
    package = with nixpkgs; nginx.override {
      modules = [ nginxModules.rtmp ];
    };

    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;


    appendConfig = ''
      rtmp_auto_push on;
      rtmp {
        server {
          listen 1935;
          chunk_size 4000;

          application tvl {
            live on;

            allow publish 88.98.195.213;
            allow publish 10.0.1.0/24;
            deny publish all;

            allow play all;
          }
        }
      }
    '';

    commonHttpConfig = ''
      log_format json_combined escape=json
      '{'
          '"remote_addr":"$remote_addr",'
          '"method":"$request_method",'
          '"uri":"$request_uri",'
          '"status":$status,'
          '"request_size":$request_length,'
          '"response_size":$body_bytes_sent,'
          '"response_time":$request_time,'
          '"referrer":"$http_referer",'
          '"user_agent":"$http_user_agent"'
      '}';

      access_log syslog:server=unix:/dev/log,nohostname json_combined;
    '';

    virtualHosts.homepage = {
      serverName = "tazj.in";
      serverAliases = [ "camden.tazj.in" ];
      default = true;
      useACMEHost = "tazj.in";
      root = depot.web.homepage;
      addSSL = true;

      extraConfig = ''
        ${depot.web.blog.oldRedirects}

        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;

        location ~* \.(webp|woff2)$ {
          add_header Cache-Control "public, max-age=31536000";
        }

        location /blog/ {
          alias ${depot.web.blog.rendered}/;

          if ($request_uri ~ ^/(.*)\.html$) {
            return 302 /$1;
          }

          try_files $uri $uri.html $uri/ =404;
        }

        location /blobs/ {
          alias /var/www/blobs/;
        }
      '';
    };

    virtualHosts.tvl = {
      serverName = "tvl.fyi";
      useACMEHost = "tvl.fyi";
      root = depot.web.tvl;
      addSSL = true;

      extraConfig = ''
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;

        location ~* \.(webp|woff2)$ {
          add_header Cache-Control "public, max-age=31536000";
        }
      '';
    };

    virtualHosts.cgit = {
      serverName = "git.tazj.in";
      serverAliases = [ "git.camden.tazj.in" ];
      useACMEHost = "tazj.in";
      addSSL = true;

      extraConfig = ''
        # Static assets must always hit the root.
        location ~ ^/(favicon\.ico|cgit\.(css|png))$ {
           proxy_pass http://localhost:2448;
        }

        # Everything else hits the depot directly.
        location / {
            proxy_pass http://localhost:2448/cgit.cgi/depot/;
        }
      '';
    };
  };

  system.stateVersion = "19.09";
})
