{ depot, ... }:

with depot.third_party;

naersk.buildPackage {
  src = ./.;

  buildInputs = [
    pkgconfig openssl systemd.dev
  ];
}
