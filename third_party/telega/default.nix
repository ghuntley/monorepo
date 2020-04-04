# Telega is an Emacs client for Telegram. It requires a native server
# component to run correctly, which is built by this derivation.
{ pkgs, ... }:

with pkgs;

stdenv.mkDerivation {
  name = "telega";
  buildInputs = [ tdlib ];

  src = fetchFromGitHub {
    owner = "zevlg";
    repo = "telega.el";
    rev = "76ffa52cd36b9ba3236d0f4e0c213495d3609212";
    sha256 = "15w6yj8n75yh6zhra4qw5pkfc8asgrwh6m8dz1vax2ma6yy2ds2r";
  } + "/server";

  installPhase = ''
    mkdir -p $out/bin
    mv telega-server $out/bin/
  '';
}
