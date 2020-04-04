# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }:

let
  # Tracking nixos-unstable as of 2020-02-17.
  commit = "82b54d490663b6d87b7b34b9cfc0985df8b49c7d";
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${commit}.tar.gz";
    sha256 = "12gpsif48g5b4ys45x36g4vdf0srgal4c96351m7gd2jsgvdllyf";
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
      cudatoolkit
      darwin
      dockerTools
      emacs26
      emacs26-nox
      emacsPackages
      emacsPackagesGen
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
    inherit (nixpkgs) git notmuch;
    ffmpeg = nixpkgs.ffmpeg-full;
  };

  # Make NixOS available
  nixos = import "${nixpkgsSrc}/nixos";
}
