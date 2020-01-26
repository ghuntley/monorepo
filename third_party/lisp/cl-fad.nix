# Portable pathname library
{ pkgs, ...}:

with pkgs.nix;

let src = pkgs.third_party.fetchFromGitHub {
  owner = "edicl";
  repo = "cl-fad";
  rev = "c13d81c4bd9ba3a172631fd05dd213ab90e7d4cb";
  sha256 = "1gc8i82v6gks7g0lnm54r4prk2mklidv2flm5fvbr0a7rsys0vpa";
};
in buildLisp.library {
  name = "cl-fad";

  deps = with pkgs.third_party.lisp; [
    alexandria
    bordeaux-threads
    (buildLisp.bundled "sb-posix")
  ];

  srcs = map (f: src + ("/" + f)) [
    "packages.lisp"
    "fad.lisp"
    "path.lisp"
    "temporary-files.lisp"
  ];
}
