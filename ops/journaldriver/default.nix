{ pkgs, ... }:

pkgs.third_party.naersk.buildPackage {
  src = ./.;

  buildInputs = with pkgs.third_party; [
    pkgconfig openssl systemd.dev    
  ];
}
