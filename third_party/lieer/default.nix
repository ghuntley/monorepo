# Lieer is a small tool to synchronise a Gmail account with a local
# maildir.
#
# Lieer is packaged in nixpkgs, but as of 2019-12-23 it is an old
# version using the previous branding (gmailieer).
{ pkgs, ... }:

# For a variety of reasons (specific to my setup), custom OAuth2
# scopes are used.
#
# The below client ID is the default for *@tazj.in and is overridden
# in a private repository for my work account. Publishing it here is
# not a security issue.
{
  clientId ? "515965513093-7b4bo4gm0q09ccsmikkuaas9a40j0jcj.apps.googleusercontent.com",
  clientSecret ? "3jVbpfT4GmubFD64svctJSdQ",
  project ? "tazjins-infrastructure"
}:

with pkgs;

let
  authPatch = runCommand "client_secret.patch" {} ''
    export CLIENT_ID='${clientId}'
    export CLIENT_SECRET='${clientSecret}'
    export PROJECT_ID='${project}'
    cat ${./api_client.patch} | ${gettext}/bin/envsubst > $out
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

  patches = [
    authPatch
    ./send_scope.patch
  ];

  propagatedBuildInputs = with python3Packages; [
    notmuch
    oauth2client
    google_api_python_client
    tqdm
  ];
}
