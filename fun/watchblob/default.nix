{ depot, ... }:

depot.nix.buildGo.program {
  name = "watchblob";
  srcs = [
    ./main.go
    ./urls.go
  ];

  deps = with depot.third_party; [
    gopkgs."golang.org".x.crypto.ssh.terminal.gopkg
  ];
}
