# Use the upstream git derivation (there's a lot of stuff happening in
# there!) and just override the source:
{ depot, ... }:

with depot.third_party;

(originals.git.overrideAttrs(_: {
  version = "2.23.0";
  src = ./.;
  doInstallCheck = false;
  preConfigure = ''
    ${autoconf}/bin/autoreconf -i
  '';
})).override {
  sendEmailSupport = true;
}
