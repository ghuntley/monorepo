{ pkgs, ... }:

pkgs.buildGo.external {
  path = "golang.org/x/time";

  src = builtins.fetchGit {
    url = "https://go.googlesource.com/time";
    rev = "555d28b269f0569763d25dbe1a237ae74c6bcc82";
  };
}
