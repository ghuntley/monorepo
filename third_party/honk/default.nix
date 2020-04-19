{ pkgs, ... }:

pkgs.stdenv.mkDerivation rec {
  name = "honk-${version}";
  version = "0.9.0";
  nativeBuildInputs = [ pkgs.go ];
  buildInputs = [ pkgs.sqlite ];

  src = builtins.fetchTarball {
    url = "https://humungus.tedunangst.com/r/honk/d/honk-${version}.tgz";
    sha256 = "0fj1ybhsra626q5vy1sy9aigxx5rjda5mgq74m7kzw7an4z2a67m";
  };

  # Go tooling needs $HOME to exist because, well, who knows.
  preBuild = ''
    mkdir home && export HOME=$PWD/home
  '';

  installPhase = ''
    install -D honk $out/bin/honk
    install -D docs/honk.1 $out/share/man/man1/honk.1
    install -D docs/honk.3 $out/share/man/man3/honk.3
    install -D docs/honk.5 $out/share/man/man5/honk.5
    install -D docs/honk.8 $out/share/man/man8/honk.8
  '';
}
