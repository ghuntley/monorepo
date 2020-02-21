{ depot, ... }:

depot.nix.buildGo.program {
  name = "sync-gcsr";
  srcs = [ ./main.go ];

  deps = with depot.third_party; map (p: p.gopkg) [
    gopkgs."gopkg.in".src-d.go-git
  ];

  x_defs = {
    "main.BuildManifest" = "${./manifest.yaml}";
  };
}
