# Common Lisp bindings to OpenSSL
{ pkgs, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/cl-plus-ssl/cl-plus-ssl.git";
  rev = "29081992f6d7b4e3aa2c5eeece4cd92b745071f4";
};
in pkgs.nix.buildLisp.library {
  name = "cl-plus-ssl";
  deps = with pkgs.third_party.lisp; [
    alexandria
    bordeaux-threads
    cffi
    flexi-streams
    sb-posix
    trivial-features
    trivial-garbage
    trivial-gray-streams
    uiop
  ];

  native = [ pkgs.third_party.openssl ];

  srcs = map (f: src + ("/src/" + f)) [
    "package.lisp"
    "reload.lisp"
    "conditions.lisp"
    "ffi.lisp"
    "x509.lisp"
    "ffi-buffer-all.lisp"
    "ffi-buffer.lisp"
    "streams.lisp"
    "bio.lisp"
    "random.lisp"
    "context.lisp"
    "verify-hostname.lisp"
  ];
}
