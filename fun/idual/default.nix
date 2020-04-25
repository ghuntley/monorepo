{ pkgs, lib, ... }:

let
  inherit (pkgs) python python3 python3Packages;

  opts = {
    pname   = "idualctl";
    version = "0.1";
    src     = ./.;

    propagatedBuildInputs = [
      python.broadlink
    ];
  };
in lib.fix (self: {
  package = python3Packages.buildPythonPackage opts;
  script  = python3Packages.buildPythonApplication opts;
  python  = python3.withPackages (_: [ self.package ]);
})
