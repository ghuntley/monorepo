# Flexible bivalent streams for Common Lisp
{ depot, ... }:

let src = builtins.fetchGit {
  url = "https://github.com/edicl/flexi-streams.git";
  rev = "0fd872ae32022e834ef861a67d86879cf33a6b64";
};
in depot.nix.buildLisp.library {
  name = "flexi-streams";
  deps = [ depot.third_party.lisp.trivial-gray-streams ];

  srcs = map (f: src + ("/" + f)) [
    "packages.lisp"
    "mapping.lisp"
    "ascii.lisp"
    "koi8-r.lisp"
    "iso-8859.lisp"
    "code-pages.lisp"
    "specials.lisp"
    "util.lisp"
    "conditions.lisp"
    "external-format.lisp"
    "length.lisp"
    "encode.lisp"
    "decode.lisp"
    "in-memory.lisp"
    "stream.lisp"
    "output.lisp"
    "input.lisp"
    "io.lisp"
    "strings.lisp"
 ];
}

