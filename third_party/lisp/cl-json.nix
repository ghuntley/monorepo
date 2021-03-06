# JSON encoder & decoder
{ depot, ... }:

with depot.nix;
let src = depot.third_party.fetchFromGitHub {
  owner = "hankhero";
  repo = "cl-json";
  rev = "6dfebb9540bfc3cc33582d0c03c9ec27cb913e79";
  sha256 = "0fx3m3x3s5ji950yzpazz4s0img3l6b3d6l3jrfjv0lr702496lh";
};
in buildLisp.library {
  name = "cl-json";
  deps = [ (buildLisp.bundled "asdf") ];

  srcs = [ "${src}/cl-json.asd" ] ++
  (map (f: src + ("/src/" + f)) [
    "package.lisp"
    "common.lisp"
    "objects.lisp"
    "camel-case.lisp"
    "decoder.lisp"
    "encoder.lisp"
    "utils.lisp"
    "json-rpc.lisp"
  ]);
}
