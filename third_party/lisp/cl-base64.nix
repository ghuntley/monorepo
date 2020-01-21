# Base64 encoding for Common Lisp
{ pkgs, ... }:

let src = builtins.fetchGit {
  url = "http://git.kpe.io/cl-base64.git";
  rev = "fc62a5342445d4ec1dd44e95f7dc513473a8c89a";
};
in pkgs.nix.buildLisp.library {
  name = "cl-base64";
  srcs = [
    (src + "/package.lisp")
    (src + "/encode.lisp")
    (src + "/decode.lisp")
  ];
}


