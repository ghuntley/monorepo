{ pkgs, ... }:

pkgs.buildGo.program {
  name = "watchblob";
  srcs = [
    ./main.go
    ./urls.go
  ];

  deps = with pkgs.third_party; [
    gopkgs."golang.org".x.crypto.ssh.terminal.gopkg
  ];
}
