{ pkgs, ... }:

pkgs.nix.buildLisp.library {
  name = "dns";

  deps = with pkgs.third_party.lisp; [
    drakma
    lisp-binary
    iterate
    alexandria
    cl-json
  ];

  srcs = [
    ./package.lisp
    ./client.lisp
  ];
}
