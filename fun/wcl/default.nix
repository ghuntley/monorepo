{ depot, ... }:

depot.nix.buildLisp.program {
  name = "wc";

  srcs = [
    ./wc.lisp
  ];

  deps = with depot.third_party.lisp; [
    unix-opts
    iterate
  ];
}
