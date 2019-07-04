# This file was generated by https://github.com/kamilchm/go2nix v1.3.0
{ stdenv, buildGoPackage, fetchgit, fetchhg, fetchbzr, fetchsvn }:

buildGoPackage rec {
  name = "git-appraise-unstable-${version}";
  version = "2019-01-16";
  rev = "2261b194e7ffd6dea6145dac3d0a25e564f8e3fc";

  goPackagePath = "github.com/google/git-appraise";

  src = fetchgit {
    inherit rev;
    url = "https://github.com/google/git-appraise";
    sha256 = "0flvpn1mcmgpjmfmndyx2rnn5n5rb0344590if81x5jz11qj4x0c";
  };

  goDeps = ./deps.nix;
}