# Imported from https://github.com/didierverna/asdf-flv
{ pkgs, ... }:

pkgs.nix.buildLisp.library {
  name = "asdf-flv";
  deps = with pkgs.third_party.lisp; [ asdf ];

  srcs = [
    ./package.lisp
    ./asdf-flv.lisp
  ];
}
