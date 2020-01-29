# unix-opts is a portable command line argument parser
{ pkgs, ...}:

let
  src = pkgs.third_party.fetchFromGitHub {
    owner = "libre-man";
    repo = "unix-opts";
    rev = "b805050b074bd860edd18cfc8776fdec666ec36e";
    sha256 = "0j93dkc9f77wz1zfspm7q1scx6wwbm6jhk8vl2rm6bfd0n8scxla";
  };
in pkgs.nix.buildLisp.library {
  name = "unix-opts";

  srcs = [
    "${src}/unix-opts.lisp"
  ];
}
