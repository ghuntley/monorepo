# cl-prevalence is an implementation of object prevalence for CL (i.e.
# an in-memory database)
{ pkgs, ... }:

let src = pkgs.third_party.fetchFromGitHub {
  owner = "40ants";
  repo = "cl-prevalence";
  rev = "da3ed6c4594b1c2fca90c178c1993973c4bf16c9";
  sha256 = "0bq905hv1626dl6b7s0zn4lbdh608g1pxaljl1fda6pwp9hmj95a";
};
in pkgs.nix.buildLisp.library {
  name = "cl-prevalence";

  deps = with pkgs.third_party.lisp; [
    s-xml
    s-sysdeps
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "package.lisp"
    "serialization/serialization.lisp"
    "serialization/xml.lisp"
    "serialization/sexp.lisp"
    "prevalence.lisp"
    "managed-prevalence.lisp"
    "master-slave.lisp"
    "blob.lisp"
  ];
}
