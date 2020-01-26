# Imported from https://github.com/didierverna/asdf-flv
{ pkgs, ... }:

with pkgs.nix;
buildLisp.library {
  name = "asdf-flv";
  deps = [ (buildLisp.bundled "asdf") ];

  srcs = [
    ./package.lisp
    ./asdf-flv.lisp
  ];
}
