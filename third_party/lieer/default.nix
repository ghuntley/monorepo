# Lieer is a small tool to synchronise a Gmail account with a local
# maildir.
#
# Lieer is packaged in nixpkgs, but as of 2019-12-23 it is an old
# version using the previous branding (gmailieer).
{ pkgs, ... }:

with pkgs.third_party;

let
  # My employer does not allow third-party projects to use our email
  # accounts, but I want to use lieer for it anyways.
  #
  # To accomplish this the following derivation creates a patch that
  # replaces the API client with one that I control.
  authPatch = runCommand "client_secret.patch" {} ''
    export PATH=${lib.makeBinPath [ gettext ]}:$PATH
    export CLIENT_SECRET='${builtins.getEnv "LIEER_CLIENT_SECRET"}'
    cat ${./api_client.patch} | envsubst > $out
  '';
in python3Packages.buildPythonApplication rec {
  name = "lieer-${version}";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "gauteh";
    repo = "lieer";
    rev = "v${version}";
    sha256 = "1zzylv8xbcrh34bz0s29dawzcyx39lai8y8wk0bl4x75v1jfynvf";
  };

  patches = [ authPatch ];

  propagatedBuildInputs = with python3Packages; [
    notmuch
    oauth2client
    google_api_python_client
    tqdm
  ];
}
