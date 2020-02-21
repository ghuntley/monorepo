{ depot, ... }:

with depot.third_party;

naersk.buildPackage {
  src = ./.;
  doDoc = false;
  doCheck = false;

  override = x: {
    # bat contains syntax highlighting packages for a lot more
    # languages than what ships with syntect, and we can make use of
    # them!
    BAT_SYNTAXES = "${bat.src}/assets/syntaxes.bin";

    # LLVM packages (why are they even required?) are not found
    # automatically if added to buildInputs, hence this ...
    LIBCLANG_PATH = "${llvmPackages.libclang}/lib/libclang.so.7";
  };
}
