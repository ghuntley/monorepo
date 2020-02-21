# Implementation of RFC2388 (multipart/form-data)
{ depot, ... }:

let src = depot.third_party.fetchFromGitHub {
  owner = "jdz";
  repo = "rfc2388";
  rev = "591bcf7e77f2c222c43953a80f8c297751dc0c4e";
  sha256 = "0phh5n3clhl9ji8jaxrajidn22d3f0aq87mlbfkkxlnx2pnw694k";
};
in depot.nix.buildLisp.library {
  name = "rfc2388";

  srcs = map (f: src + ("/" + f)) [
    "packages.lisp"
    "rfc2388.lisp"
  ];
}
