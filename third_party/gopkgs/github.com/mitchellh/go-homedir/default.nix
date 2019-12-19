{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/mitchellh/go-homedir";

  src = pkgs.third_party.fetchFromGitHub {
    owner = "mitchellh";
    repo = "go-homedir";
    rev = "af06845cf3004701891bf4fdb884bfe4920b3727";
    sha256 = "0ydzkipf28hwj2bfxqmwlww47khyk6d152xax4bnyh60f4lq3nx1";
  };
}
