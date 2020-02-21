# Hunchentoot is a web framework for Common Lisp.
{ depot, ...}:

let
  src = depot.third_party.fetchFromGitHub {
    owner = "edicl";
    repo = "hunchentoot";
    rev = "585b45b6b873f2da421fdf456b61860ab5868207";
    sha256 = "13nazwix067mdclq9vgjhsi2vpr57a8dz51dd5d3h99ccsq4mik5";
  };
  url-rewrite = depot.nix.buildLisp.library {
    name = "url-rewrite";

    srcs = map (f: src + ("/url-rewrite/" + f)) [
      "packages.lisp"
      "specials.lisp"
      "primitives.lisp"
      "util.lisp"
      "url-rewrite.lisp"
    ];
  };
in depot.nix.buildLisp.library {
  name = "hunchentoot";

  deps = with depot.third_party.lisp; [
    alexandria
    bordeaux-threads
    chunga
    cl-base64
    cl-fad
    rfc2388
    cl-plus-ssl
    cl-ppcre
    flexi-streams
    md5
    trivial-backtrace
    usocket
    url-rewrite
  ];

  srcs = map (f: src + ("/" + f)) [
    "hunchentoot.asd"
    "packages.lisp"
    "compat.lisp"
    "specials.lisp"
    "conditions.lisp"
    "mime-types.lisp"
    "util.lisp"
    "log.lisp"
    "cookie.lisp"
    "reply.lisp"
    "request.lisp"
    "session.lisp"
    "misc.lisp"
    "headers.lisp"
    "set-timeouts.lisp"
    "taskmaster.lisp"
    "acceptor.lisp"
    "easy-handlers.lisp"
  ];
}
