{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/xanzy/ssh-agent";

  src = pkgs.third_party.fetchFromGitHub {
    owner = "xanzy";
    repo = "ssh-agent";
    rev = "6a3e2ff9e7c564f36873c2e36413f634534f1c44";
    sha256 = "1chjlnv5d6svpymxgsr62d992m2xi6jb5lybjc5zn1h3hv1m01av";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."golang.org".x.crypto.ssh.agent
  ];
}
