# Drakma is an HTTP client for Common Lisp.
{ pkgs, ... }:

with pkgs.nix;

let src = pkgs.third_party.fetchFromGitHub {
  owner = "edicl";
  repo = "drakma";
  rev = "87feb02bef00b11a753d5fb21a5fec526b0d0bbb";
  sha256 = "01b80am2vrw94xmdj7f21qm7p5ys08mmpzv4nc4icql81hqr1w2m";
};
in buildLisp.library {
  name = "drakma";
  deps = with pkgs.third_party.lisp; [
    chipz
    chunga
    cl-base64
    cl-plus-ssl
    cl-ppcre
    flexi-streams
    puri
    usocket
    (buildLisp.bundled "asdf")
  ];

  srcs = map (f: src + ("/" + f)) [
    "drakma.asd" # Required because the system definition is used
    "packages.lisp"
    "specials.lisp"
    "conditions.lisp"
    "util.lisp"
    "read.lisp"
    "cookies.lisp"
    "encoding.lisp"
    "request.lisp"
  ];
}
