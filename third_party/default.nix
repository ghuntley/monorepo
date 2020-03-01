# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }:

let
  # Tracking nixos-unstable as of 2020-02-17.
  commit = "ea79a830dcf9c0059656da7f52835d2663d5c436";
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${commit}.tar.gz";
    sha256 = "0vqnfh99358v9ym5z9i3dsfy0l4xxgh9hr278pi1y11gdl092014";
  };
  nixpkgs = import nixpkgsSrc {
    config.allowUnfree = true;
    config.allowBroken = true;
  };

  exposed = {
    # Inherit the packages from nixpkgs that should be available inside
    # of the repo. They become available under `pkgs.third_party.<name>`
    inherit (nixpkgs)
      age
      autoconf
      bashInteractive
      bat
      buildGoModule
      buildGoPackage
      bzip2
      c-ares
      cacert
      cachix
      cargo
      cgit
      clang_9
      cmake
      coreutils
      darwin
      dockerTools
      emacs26
      emacs26-nox
      emacsPackagesNg
      emacsPackagesNgGen
      fetchFromGitHub
      fetchurl
      fetchzip
      fira
      fira-code
      fira-mono
      gettext
      glibc
      gnutar
      go
      google-cloud-sdk
      grpc
      gzip
      haskell
      iana-etc
      imagemagickBig
      jq
      kontemplate
      lib
      libredirect
      llvmPackages
      luajit
      luatex
      makeFontsConf
      makeWrapper
      mdbook
      mime-types
      moreutils
      nano
      nginx
      nix
      openssh
      openssl
      overrideCC
      overrideCCC
      pandoc
      parallel
      pkgconfig
      pounce
      protobuf
      python3Packages
      remarshal
      rink
      ripgrep
      rsync
      runCommand
      runCommandNoCC
      rustPlatform
      rustc
      sbcl
      stdenv
      stern
      symlinkJoin
      systemd
      tdlib
      terraform_0_12
      texlive
      thttpd
      tree
      writeShellScript
      writeShellScriptBin
      writeText
      writeTextFile
      xz
      zlib
      zstd;
  };

in exposed // {
  callPackage = nixpkgs.lib.callPackageWith exposed;

  # Provide the source code of nixpkgs, but do not provide an imported
  # version of it.
  inherit nixpkgsSrc;

  # Packages to be overridden
  originals = {
    inherit (nixpkgs) git guile notmuch;
  };

  # Make NixOS available
  nixos = import "${nixpkgsSrc}/nixos";
}
