# Common Lisp library for decompressing deflate, zlib, gzip, and bzip2 data
{ pkgs, ... }:

let src = pkgs.third_party.fetchFromGitHub {
  owner = "froydnj";
  repo = "chipz";
  rev = "75dfbc660a5a28161c57f115adf74c8a926bfc4d";
  sha256 = "0plx4rs39zbs4gjk77h4a2q11zpy75fh9v8hnxrvsf8fnakajhwg";
};
in pkgs.nix.buildLisp.library {
  name = "chipz";
  deps = with pkgs.third_party.lisp; [ asdf ];

  srcs = map (f: src + ("/" + f)) [
    "chipz.asd"
    "package.lisp"
    "constants.lisp"
    "conditions.lisp"
    "dstate.lisp"
    "types-and-tables.lisp"
    "crc32.lisp"
    "adler32.lisp"
    "inflate-state.lisp"
    "gzip.lisp"
    "zlib.lisp"
    "inflate.lisp"
    "bzip2.lisp"
    "decompress.lisp"
    "stream.lisp"
  ];
}
