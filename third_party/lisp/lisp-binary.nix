# A library to easily read and write complex binary formats.
{ pkgs, ... }:

let src = pkgs.third_party.fetchFromGitHub {
  owner = "j3pic";
  repo = "lisp-binary";
  rev = "1aefc8618b7734f68697ddf59bc93cb8522aa0bf";
  sha256 = "1hflzn3mjp32jz9fxx9wayp3c3x58s77cgjfbs06nrynqkv0c6df";
};
in pkgs.nix.buildLisp.library {
  name = "lisp-binary";

  deps = with pkgs.third_party.lisp; [
    cffi
    quasiquote_2
    moptilities
    flexi-streams
    closer-mop
  ];

  srcs = map (f: src + ("/" + f)) [
    "utils.lisp"
    "integer.lisp"
    "float.lisp"
    "simple-bit-stream.lisp"
    "reverse-stream.lisp"
    "binary-1.lisp"
    "binary-2.lisp"
  ];
}
