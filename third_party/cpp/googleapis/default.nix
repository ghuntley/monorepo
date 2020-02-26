# This library contains generated gRPC implementations for Google's
# public libraries.
{ pkgs, ... }:

let
  inherit (pkgs) fetchFromGitHub;
  stdenv = with pkgs; overrideCC pkgs.stdenv clang_9;
in stdenv.mkDerivation {
  name = "googleapis-cpp";
  src = ./.;

  GOOGLEAPIS_DIR = fetchFromGitHub {
    owner = "googleapis";
    repo = "googleapis";
    rev = "0aba1900ffef672ec5f0da677cf590ee5686e13b";
    sha256 = "1174mvipmzap4h8as1cl44y1kq7ikipdicnmnswv5yswgkwla84c";
  };

  buildInputs = with pkgs; [
    c-ares c-ares.cmake-config grpc protobuf openssl zlib
  ];

  nativeBuildInputs = with pkgs; [ cmake pkgconfig ];
}
