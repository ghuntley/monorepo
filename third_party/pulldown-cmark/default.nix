{ pkgs, ... }:

with pkgs.third_party;

naersk.buildPackage {
  src = fetchFromGitHub {
    owner = "raphlinus";
    repo = "pulldown-cmark";
    rev = "e10b6801081799cab52dc3029a6de357b604f13e";
    sha256 = "1zibwh9fnysgkg2sgp6fxlsbc1cwp5gqc8nqsdglry9p21jd351i";
  };
}
