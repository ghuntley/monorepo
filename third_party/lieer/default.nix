# Lieer is a small tool to synchronise a Gmail account with a local
# maildir.
#
# Lieer is packaged in nixpkgs, but as of 2019-12-23 it is an old
# version using the previous branding (gmailieer).
{ pkgs, ... }:

with pkgs.third_party;

python3Packages.buildPythonApplication rec {
  name = "lieer-${version}";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "gauteh";
    repo = "lieer";
    rev = "v${version}";
    sha256 = "1zzylv8xbcrh34bz0s29dawzcyx39lai8y8wk0bl4x75v1jfynvf";
  };

  propagatedBuildInputs = with python3Packages; [
    notmuch
    oauth2client
    google_api_python_client
    tqdm
  ];
}
