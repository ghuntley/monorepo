# Compatibility layer for minor MOP implementation differences
{ depot, ... }:

let src = depot.third_party.fetchFromGitHub {
  owner = "gwkkwg";
  repo = "moptilities";
  rev = "a436f16b357c96b82397ec018ea469574c10dd41";
  sha256 = "1q12bqjbj47lx98yim1kfnnhgfhkl80102fkgp9pdqxg0fp6g5fc";
};
in depot.nix.buildLisp.library {
  name = "moptilities";
  deps = [ depot.third_party.lisp.closer-mop ];
  srcs = [ "${src}/dev/moptilities.lisp" ];
}
