# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.

{ pkgs, ... }:
let
  # Tracking nixos-unstable as of 2020-01-18.
  commit = "c438ce12a858f24c1a2479213eaab751da45fa50";
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${commit}.tar.gz";
    sha256 = "18m4hxx8y0gfrmhkz29iyc0hmss584m9xhgpk7j7bwjaci0fps4z";
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
      buildGoPackage
      bzip2
      cacert
      cachix
      cargo
      cgit
      coreutils
      darwin
      dockerTools
      emacs26
      emacs26-nox
      emacsPackagesNg
      emacsPackagesNgGen
      fetchFromGitHub
      fetchurl
      fira
      fira-code
      fira-mono
      gettext
      glibc
      gnutar
      go
      google-cloud-sdk
      gzip
      haskell
      iana-etc
      imagemagickBig
      jq
      kontemplate
      lib
      libredirect
      lispPackages
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
      parallel
      pkgconfig
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
