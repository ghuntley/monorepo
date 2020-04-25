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
  package = python3Packages.buildPythonPackage opts;
  script  = python3Packages.buildPythonApplication opts;
in {
  inherit script;
  python  = python3.withPackages (_: [ package ]);
  setAlarm = pkgs.writeShellScriptBin "set-alarm" ''
    echo "setting an alarm for ''${1}"
    ${pkgs.systemd}/bin/systemd-run --user --on-calendar="''${1} Europe/London" --unit=light-alarm.service
  '';
}
