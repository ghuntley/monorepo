# Portable chunked streams for Common Lisp
{ depot, ... }:

let src = depot.third_party.fetchFromGitHub {
  owner = "edicl";
  repo = "chunga";
  rev = "16330852d01dfde4dd97dee7cd985a88ea571e7e";
  sha256 = "0jzn3nyb3f22gm983rfk99smqs3mhb9ivjmasvhq9qla5cl9pyhd";
};
in depot.nix.buildLisp.library {
  name = "chunga";
  deps = with depot.third_party.lisp; [
    trivial-gray-streams
  ];

  srcs = map (f: src + ("/" + f)) [
    "packages.lisp"
    "specials.lisp"
    "util.lisp"
    "known-words.lisp"
    "conditions.lisp"
    "read.lisp"
    "streams.lisp"
    "input.lisp"
    "output.lisp"
  ];
}
