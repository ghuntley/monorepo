{ pkgs, ... }:

pkgs.buildGo.external {
  path = "gopkg.in/warnings.v0";

  src = pkgs.third_party.fetchFromGitHub {
    owner = "go-warnings";
    repo = "warnings";
    rev = "27b9fabbdaf131d2169ec3ff7db8ffc4d839635e";
    sha256 = "1y276jd9gwvjriz8yd98k3srgbnmbja8f7f7m6lvr0h5sbq3g3w9";
  };
}
