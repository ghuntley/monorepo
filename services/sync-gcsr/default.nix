{ pkgs, ... }:

pkgs.buildGo.program {
  name = "sync-gcsr";
  srcs = [ ./main.go ];

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."gopkg.in".src-d.go-git
  ];
}
