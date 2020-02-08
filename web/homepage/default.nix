# Assembles the website index and configures an nginx instance to
# serve it.
#
# The website is made up of a simple header&footer and content
# elements for things such as blog posts and projects.
#
# Content for the blog is in //web/blog instead of here.
{ pkgs, lib, ... }:

with pkgs;
with nix.yants;

third_party.callPackage ./nginx.nix {
  blog = web.blog;
}
