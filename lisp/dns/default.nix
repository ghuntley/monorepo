{ pkgs, ... }:

pkgs.nix.buildLisp.library {
  name = "dns";

  deps = with pkgs.third_party.lisp; [
    alexandria
    cl-json
    drakma
  ];

  srcs = [
    ./resolver.lisp
  ];
}
