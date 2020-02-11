# NixOS module for Tailscale
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.tailscale;

  aclVar = optionalAttrs (cfg.aclFile != null) {
    ACL_FILE = "--acl-file=${cfg.aclFile}";
  };

in {
  options.services.tailscale = {
    enable = mkEnableOption "Tailscale relay";

    package = mkOption {
      type = types.package;
      default = pkgs.tailscale; # <- this doesn't actually exist yet
      description = "Tailscale client package to use";
    };

    port = mkOption {
      type = types.int;
      default = 41641;
      description = ''
        Set the port to listen on for incoming VPN packets.

        Remote nodes will automatically be informed about the new port
        number, but you might want to configure this in order to set
        external firewall settings.
      '';
    };

    aclFile = mkOption {
      type = with types; nullOr path;
      default = "${cfg.package}/etc/acl.json";
    };

    relayConf = mkOption {
      type = types.path;
      example = "/etc/tailscale.conf";
      description = "The path to relay.conf";
    };

    extraFlags = mkOption {
      type = with types; listOf str;
      default = [];
      description = "Extra flags you might want to pass to relaynode.";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    systemd.services.tailscale-relay = {
      description = "Traffic relay node for Tailscale IPN";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = with pkgs; [ iproute iptables ];

      unitConfig.ConditionPathExists = cfg.relayConf;

      script = concatStringsSep " " ([
        "${cfg.package}/bin/relaynode"
        "--port=${toString cfg.port}"
        "--config=${cfg.relayConf}"
        (optionalString (cfg.aclFile != null) "--acl-file=${cfg.aclFile}")
      ] ++ cfg.extraFlags);

      serviceConfig = {
        RuntimeDirectory = "tailscale";
        LogsDirectory = "tailscale";
      };
    };
  };
}
