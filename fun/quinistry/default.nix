{ depot, ... }:

depot.nix.buildGo.program {
  name = "quinistry";
  srcs = [
    ./const.go
    ./image.go
    ./main.go
    ./types.go
  ];
}
