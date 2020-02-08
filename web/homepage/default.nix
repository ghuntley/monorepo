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

let
  inherit (builtins) readFile replaceStrings sort;
  inherit (third_party) writeFile runCommandNoCC;

  # The different types of entries on the homepage.
  entryClass = enum "entryClass" [ "blog" "project" "misc" ];

  # The definition of a single entry.
  entry = struct "entry" {
    class = entryClass;
    title = string;
    url = string;
    date = int; # epoch
    description = option string;
  };

  escape = replaceStrings [ "<" ">" "&" "'" ] [ "&lt;" "&gt;" "&amp;" "&#39;" ];

  postToEntry = defun [ web.blog.post entry ] (post: {
    class = "blog";
    title = "Blog: " + post.title;
    url = "/blog/${post.key}";
    date = post.date;
  });

  # TODO(tazjin): add date formatting function
  entryToDiv = defun [ entry string ] (entry: ''
    <div class="entry ${entry.class}">
      <p class="entry-title">${escape entry.title}</p>
      ${
        lib.optionalString ((entry ? description) && (entry.description != null))
        "<p class=\"entry-description\">${escape entry.description}</p>"
      }
    </div>
  '');

  index = entries: third_party.writeText "index.html" (lib.concatStrings (
    [ (builtins.readFile ./header.html) ]
    ++ (map entryToDiv (sort (a: b: a.date < b.date) entries))
    ++ [ (builtins.readFile ./footer.html) ]
  ));

  homepage = index ((map postToEntry web.blog.posts) ++ (import ./entries.nix));
  website = runCommandNoCC "website" {} ''
    mkdir $out
    cp ${homepage} $out/index.html
    cp -r ${./static} $out/static
  '';
in third_party.callPackage ./nginx.nix {
  inherit website;
  blog = web.blog;
}
