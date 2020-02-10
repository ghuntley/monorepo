# This file packages the Tailscale client using the standard upstream
# Go packaging mechanisms instead of buildGo.nix

{ pkgs, lib, ... }:

let
  inherit (pkgs.third_party) buildGoModule fetchFromGitHub;
in buildGoModule {
  pname = "tailscale";
  version = "fef25489";

  src = fetchFromGitHub {
    owner = "tailscale";
    repo = "tailscale";
    rev = "fef254898178d100f25b98530499adcf07cfded3";
    sha256 = "1islxzr8lhnl2f0r686mcciwb8lzvqjczg9fs0nagr5pp6dsi9fa";
  };

  goPackagePath = "tailscale.com";
  modSha256 = "0cnih9flwgqjq4x4cwyac9yyz1prv2i2by1ki3g71ai8q621bq10";
  subPackages = [
    "cmd/relaynode"
    "cmd/taillogin"
    "cmd/tailscale"
    "cmd/tailscaled"
  ];

  meta = with lib; {
    homepage = "https://tailscale.com/";
    description = "Private WireGuard networks made easy";
    license = licenses.bsd3;
    maintainers = with maintainers; [ tazjin ];
  };
}
