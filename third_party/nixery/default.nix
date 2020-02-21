# Technically I suppose Nixery is not a third-party program, but it's
# outside of this repository ...
{ pkgs, ... }:

let src = pkgs.fetchFromGitHub {
  owner = "google";
  repo = "nixery";
  rev = "4f6ce83f9296545d6c74321b37d18545764c8827";
  sha256 = "19aiak1pss6vwm0fwn02827l5ir78fkqglfbdl2gchsyv3gps8bg";
};
in import src {
  inherit pkgs;
  preLaunch = ''
    export USER=root
    cachix use tazjin
  '';
  extraPackages = with pkgs.third_party; [ cachix openssh ];
}
