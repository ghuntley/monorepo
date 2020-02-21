{ depot, ... }:

depot.buildGo.external {
  path = "golang.org/x/crypto";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/crypto";
    rev = "e9b2fee46413994441b28dfca259d911d963dfed";
  };

  deps = with depot.third_party; [
    gopkgs."golang.org".x.sys.unix.gopkg
  ];
}
