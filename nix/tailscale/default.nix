# This file defines a Nix helper function to create Tailscale ACL files.
#
# https://tailscale.com/kb/1018/install-acls

{ pkgs, ... }:

with pkgs.nix.yants;

let
  inherit (builtins) toFile toJSON;

  entry = struct "aclEntry" {
    Action = enum [ "accept" "reject" ];
    Users = list string;
    Ports = list string;
  };

  acl = list entry;
in entries: toFile "tailscale-acl.json" (toJSON (acl entries))
