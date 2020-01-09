# Babel is an encoding conversion library for Common Lisp.
{ pkgs, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/cl-babel/babel.git";
  rev = "ec9a17cdbdba3c1dd39609fc7961cfb3f0aa260e";
};
in pkgs.nix.buildLisp.library {
  name = "babel";
  deps = [ pkgs.third_party.lisp.alexandria ];

  srcs = map (f: src + ("/src/" + f)) [
    "packages.lisp"
    "encodings.lisp"
    "enc-ascii.lisp"
    "enc-ebcdic.lisp"
    "enc-ebcdic-int.lisp"
    "enc-iso-8859.lisp"
    "enc-unicode.lisp"
    "enc-cp1251.lisp"
    "enc-cp1252.lisp"
    "jpn-table.lisp"
    "enc-jpn.lisp"
    "enc-gbk.lisp"
    "enc-koi8.lisp"
    "external-format.lisp"
    "strings.lisp"
    "gbk-map.lisp"
    "sharp-backslash.lisp"
  ];
}
