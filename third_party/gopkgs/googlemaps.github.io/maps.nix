{ pkgs, ... }:

pkgs.buildGo.external {
  path = "googlemaps.github.io/maps";

  src = pkgs.third_party.fetchFromGitHub {
    owner = "googlemaps";
    repo = "google-maps-services-go";
    rev = "a46d9fca56ac82caa79408b2417ea93a75e3b986";
    sha256 = "1zpl85yd3m417060isdlhxzakqkf4f59jgpz3kcjp2i0mkrskkjs";
  };

  deps = with pkgs.third_party; map (p: p.gopkg) [
    gopkgs."github.com".google.uuid
    gopkgs."golang.org".x.time.rate
  ];
}
