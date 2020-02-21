# Imported from https://github.com/didierverna/asdf-flv
{ depot, ... }:

with depot.nix;
buildLisp.library {
  name = "asdf-flv";
  deps = [ (buildLisp.bundled "asdf") ];

  srcs = [
    ./package.lisp
    ./asdf-flv.lisp
  ];
}
