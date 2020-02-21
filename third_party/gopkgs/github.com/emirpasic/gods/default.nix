{ depot, ... }:

depot.buildGo.external {
  path = "github.com/emirpasic/gods";

  src = depot.third_party.fetchFromGitHub {
    owner = "emirpasic";
    repo = "gods";
    rev = "4e23915b9a82f35f320a68a395a7a5045c826932";
    sha256 = "00f8ch1rccakc62f9nj97hapvnx84z7wbcdmbmz7p802b9mxk5nl";
  };
}
