{ pkgs, ... }:

pkgs.nix.buildLisp.program {
  name = "wc";

  srcs = [
    ./wc.lisp
  ];

  deps = with pkgs.third_party.lisp; [
    iterate
  ];
}
