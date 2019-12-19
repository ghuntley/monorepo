{ pkgs, ... }:

pkgs.buildGo.external {
  path = "golang.org/x/crypto";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/crypto";
    rev = "e9b2fee46413994441b28dfca259d911d963dfed";
  };

  deps = with pkgs.third_party; [
    # gopkgs."golang.org".x.text.secure.bidirule.gopkg
    # gopkgs."golang.org".x.text.unicode.bidi.gopkg
    # gopkgs."golang.org".x.text.unicode.norm.gopkg
  ];
}
