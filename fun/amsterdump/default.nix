{ pkgs, ... }:

pkgs.buildGo.program {
  name = "amsterdump";
  srcs = [
    ./main.go
  ];

  deps = with pkgs.third_party; map (p: p.gopkg) [
    # gopkgs."golang.org".x.oauth2.google
    gopkgs."googlemaps.github.io".maps
  ];
}
