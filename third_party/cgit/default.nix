{ depot, ... }:

let
  inherit (depot.third_party) stdenv gzip bzip2 xz luajit zlib autoconf openssl pkgconfig;
in stdenv.mkDerivation rec {
  pname = "cgit";
  version = "master";
  src = ./.;

  nativeBuildInputs = [ autoconf pkgconfig ];
  buildInputs = [ openssl zlib luajit ];

  postPatch = ''
    sed -e 's|"gzip"|"${gzip}/bin/gzip"|' \
        -e 's|"bzip2"|"${bzip2.bin}/bin/bzip2"|' \
        -e 's|"xz"|"${xz.bin}/bin/xz"|' \
        -i ui-snapshot.c
  '';

  # Give cgit the git source tree from the depot. Note that the
  # versions should be kept in sync (see the Makefile for the current
  # git version).
  preBuild = ''
    rm -rf git # remove submodule dir ...
    cp -r --no-preserve=ownership,mode ${depot.third_party.git.src} git
    makeFlagsArray+=(prefix="$out" CGIT_SCRIPT_PATH="$out/cgit/")
  '';

  meta = {
    homepage = https://git.zx2c4.com/cgit/about/;
    repositories.git = git://git.zx2c4.com/cgit;
    description = "Web frontend for git repositories";
    license = stdenv.lib.licenses.gpl2;
    platforms = stdenv.lib.platforms.linux;
    maintainers = with stdenv.lib.maintainers; [ bjornfor ];
  };
}
