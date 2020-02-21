{ pkgs, ... }:

{ name, src, deps ? (_: []), emacs ? pkgs.emacs26-nox }:

let
  inherit (pkgs) emacsPackagesNg emacsPackagesNgGen writeTextFile;
  inherit (builtins) isString toFile;

  finalEmacs = (emacsPackagesNgGen emacs).emacsWithPackages deps;

  srcFile = if isString src
    then toFile "${name}.el" src
    else src;
in writeTextFile {
  inherit name;
  executable = true;
  destination = "/bin/${name}";

  text = ''
    #!/bin/sh
    ${finalEmacs}/bin/emacs --batch --no-site-file --script ${srcFile} $@
  '';
}
