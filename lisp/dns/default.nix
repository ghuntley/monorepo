{ pkgs, ... }:

pkgs.nix.buildLisp.library {
  name = "dns";

  deps = with pkgs.third_party.lisp; [
    drakma
    lisp-binary
    iterate
  ];

  srcs = [
    ./package.lisp
    ./message.lisp
    ./client.lisp
  ];
}
