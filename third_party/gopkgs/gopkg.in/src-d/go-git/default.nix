{ depot, ... }:

depot.buildGo.external {
  # .v4 is used throughout the codebase and I can't be bothered to do
  # anything else about it other than using that package path here.
  path = "gopkg.in/src-d/go-git.v4";

  src = depot.third_party.fetchFromGitHub {
    owner = "src-d";
    repo = "go-git";
    rev = "1a7db85bca7027d90afdb5ce711622aaac9feaed";
    sha256 = "08jl4ljrzzil7c3qcl2y1859nhpgw9ixxy1g40ff7kmq989yhs6v";
  };

  deps = with depot.third_party; map (p: p.gopkg) [
    gopkgs."github.com".emirpasic.gods.trees.binaryheap
    gopkgs."github.com".jbenet.go-context.io
    gopkgs."github.com".kevinburke.ssh_config
    gopkgs."github.com".mitchellh.go-homedir
    gopkgs."github.com".sergi.go-diff.diffmatchpatch
    gopkgs."github.com".src-d.gcfg
    gopkgs."github.com".xanzy.ssh-agent
    gopkgs."golang.org".x.crypto.openpgp
    gopkgs."golang.org".x.crypto.ssh
    gopkgs."golang.org".x.crypto.ssh.knownhosts
    gopkgs."golang.org".x.net.proxy
    gopkgs."gopkg.in".src-d.go-billy
    gopkgs."gopkg.in".src-d.go-billy.osfs
    gopkgs."gopkg.in".src-d.go-billy.util
  ];
}
