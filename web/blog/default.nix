# This creates the static files that make up my blog from the Markdown
# files in this repository.
#
# All blog posts are rendered from Markdown by cheddar.
{ pkgs, lib, ... }@args:

with pkgs.nix.yants;

let
  # Type definition for a single blog post.
  post = struct "blog-post" {
    key = string; #
    title = string;
    date = string; # *sigh*

    # Path to the Markdown file containing the post content.
    content = path;

    # Should this post be included in the index? (defaults to true)
    listed = option bool;

    # Is this a draft? (adds a banner indicating that the link should
    # not be shared)
    draft = option bool;

    # Previously each post title had a numeric ID. For these numeric
    # IDs, redirects are generated so that old URLs stay compatible.
    oldKey = option string;
  };

  posts = list post (import ./posts.nix);
  fragments = import ./fragments.nix args;

  renderedBlog = pkgs.third_party.runCommandNoCC "tazjins-blog" {} ''
    mkdir -p $out

    cp ${fragments.blogIndex posts} $out/index.html

    ${lib.concatStringsSep "\n" (map (post:
      "cp ${fragments.renderPost post} $out/${post.key}.html"
    ) posts)}
  ''; # '' (this line makes nix-mode happy :/)

in import ./nginx.nix (args // {
  inherit posts renderedBlog;
})