{ depot, ... }:

depot.nix.buildLisp.library {
  name = "tazblog";

  deps =
  # Local dependencies
  (with pkgs.lisp; [ dns ])

  # Third-party dependencies
  ++ (with pkgs.third_party.lisp; [
    cl-base64
    cl-json
    hunchentoot
    iterate
  ]);

  srcs = [
    ./store.lisp
  ];
}
