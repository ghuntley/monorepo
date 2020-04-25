# Python package for controlling the Broadlink RM Pro Infrared
# controller.
#
# https://github.com/mjg59/python-broadlink
{ pkgs, lib, ... }:

let
  inherit (pkgs) fetchFromGitHub;
  inherit (pkgs.python3Packages) buildPythonPackage cryptography;
in buildPythonPackage (lib.fix (self: {
  pname = "python-broadlink";
  version = "0.13.2";

  src = fetchFromGitHub {
    owner = "mjg59";
    repo = "python-broadlink";
    rev = self.version;
    sha256 = "0dwqzx294sjdc8dg1sd3z6mhll0zzsb8k0lzs63nyhm2nc9pyv6j";
  };

  propagatedBuildInputs = [ cryptography ];
}))
