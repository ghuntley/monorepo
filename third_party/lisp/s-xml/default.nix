# XML serialiser for Common Lisp.
#
# This system was imported from a Quicklisp tarball at 's-xml-20150608'.
{ pkgs, ... }:

pkgs.nix.buildLisp.library {
  name = "s-xml";

  srcs = [
    ./src/package.lisp
    ./src/xml.lisp
    ./src/dom.lisp
    ./src/lxml-dom.lisp
    ./src/sxml-dom.lisp
    ./src/xml-struct-dom.lisp
  ];
}
