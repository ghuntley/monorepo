{ depot, ... }:

depot.nix.buildLisp.library {
  name = "dns";

  deps = with depot.third_party.lisp; [
    drakma
    lisp-binary
    iterate
  ];

  srcs = [
    ./package.lisp
    ./message.lisp
    ./client.lisp
  ];
}
