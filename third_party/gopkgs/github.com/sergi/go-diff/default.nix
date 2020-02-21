{ depot, ... }:

depot.buildGo.external {
  path = "github.com/sergi/go-diff";

  src = depot.third_party.fetchFromGitHub {
    owner = "sergi";
    repo = "go-diff";
    rev = "58c5cb1602ee9676b5d3590d782bedde80706fcc";
    sha256 = "0ir8ali2vx0j7pipmlfd6k8c973akyy2nmbjrf008fm800zcp7z2";
  };
}
