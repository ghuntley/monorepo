
{ depot, ... }:

let src = builtins.fetchGit {
  url = "https://notabug.org/cage/cl-colors2.git";
  rev = "795aedee593b095fecde574bd999b520dd03ed24";
};
in depot.nix.buildLisp.library {
  name = "cl-colors2";
  deps = with depot.third_party.lisp; [
    alexandria
    cl-ppcre
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "colors.lisp"
    "colornames.lisp"
    "hexcolors.lisp"
  ];
}
