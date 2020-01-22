# MD5 hash implementation
{ pkgs, ... }:

let src = pkgs.third_party.fetchFromGitHub {
  owner = "pmai";
  repo = "md5";
  rev = "b1412600f60d526ee34a7ba1596ec483da7894ab";
  sha256 = "0lzip6b6xg7gd70xl1xmqp24fvxqj6ywjnz9lmx7988zpj20nhl2";
};
in pkgs.nix.buildLisp.library {
  name = "md5";

  deps = with pkgs.third_party.lisp; [
    sb-rotate-byte
  ];

  srcs = [ (src + "/md5.lisp") ];
}
