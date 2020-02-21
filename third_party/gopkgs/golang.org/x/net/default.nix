{ depot, ... }:

depot.buildGo.external {
  path = "golang.org/x/net";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/net";
    rev = "c0dbc17a35534bf2e581d7a942408dc936316da4";
  };

  deps = with depot.third_party; [
    gopkgs."golang.org".x.text.secure.bidirule.gopkg
    gopkgs."golang.org".x.text.unicode.bidi.gopkg
    gopkgs."golang.org".x.text.unicode.norm.gopkg
  ];
}
