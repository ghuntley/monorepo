{ depot, ... }:

depot.buildGo.external {
  path = "github.com/src-d/gcfg";

  src = depot.third_party.fetchFromGitHub {
    owner = "src-d";
    repo = "gcfg";
    rev = "1ac3a1ac202429a54835fe8408a92880156b489d";
    sha256 = "044j95skmyrwjw5fwjk6ka32rjgsg0ar0mfp9np19sh1acwv4x4r";
  };

  deps = with depot.third_party; map (p: p.gopkg) [
    gopkgs."gopkg.in".warnings
  ];
}
