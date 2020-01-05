{ pkgs, ... }:

pkgs.buildGo.external {
  path = "github.com/google/uuid";

  src = pkgs.third_party.fetchFromGitHub {
    owner = "google";
    repo = "uuid";
    rev = "c2e93f3ae59f2904160ceaab466009f965df46d6";
    sha256 = "0zw8fvl6jqg0fmv6kmvhss0g4gkrbvgyvl2zgy5wdbdlgp4fja0h";
  };
}
