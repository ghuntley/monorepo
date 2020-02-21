{ depot, ... }:

depot.buildGo.external {
  path = "golang.org/x/text";
  src = builtins.fetchGit {
    url = "https://go.googlesource.com/text";
    rev = "cbf43d21aaebfdfeb81d91a5f444d13a3046e686";
  };
}
