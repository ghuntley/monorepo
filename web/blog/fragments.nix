# This file defines various fragments of the blog, such as the header
# and footer, as functions that receive arguments to be templated into
# them.
#
# An entire post is rendered by `renderPost`, which assembles the
# fragments together in a runCommand execution.
#
# The post index is generated by //web/homepage, not by this code.
{ depot, lib, ... }:

let
  inherit (builtins) filter map hasAttr replaceStrings toFile;
  inherit (depot.third_party) runCommandNoCC writeText;

  # Generate a post list for all listed, non-draft posts.
  isDraft = post: (hasAttr "draft" post) && post.draft;
  isUnlisted = post: (hasAttr "listed" post) && !post.listed;

  escape = replaceStrings [ "<" ">" "&" "'" ] [ "&lt;" "&gt;" "&amp;" "&#39;" ];

  header = title: ''
  <!DOCTYPE html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta name="description" content="tazjin&#39;s blog">
    <link rel="stylesheet" type="text/css" href="/static/tazjin.css" media="all">
    <link rel="icon" type="image/webp" href="/static/favicon.webp">
    <link rel="alternate" type="application/rss+xml" title="RSS-Feed" href="/rss.xml">
    <title>tazjin&#39;s blog: ${escape title}</title>
  </head>
  <body class="light">
    <header>
      <h1><a class="blog-title" href="/">tazjin&#39;s interblag</a> </h1>
      <hr>
    </header>
  '';

  footer = ''
    <hr>
    <footer>
      <p class="footer">
        <a class="uncoloured-link" href="https://tazj.in">homepage</a>
        |
        <a class="uncoloured-link" href="https://git.tazj.in/about">code</a>
        |
        <a class="uncoloured-link" href="https://twitter.com/tazjin">twitter</a>
      </p>
      <p class="lod">ಠ_ಠ</p>
    </footer>
  </body>
  '';

  draftWarning = toFile "draft.html" ''
    <p class="cheddar-callout cheddar-warning">
      <b>Note:</b> This post is a <b>draft</b>! Please do not share
      the link to it without asking me first.
    </p>
    <hr>
  '';

  unlistedWarning = toFile "unlisted.html" ''
    <p class="cheddar-callout cheddar-warning">
      <b>Note:</b> This post is <b>unlisted</b>! Please do not share
      the link to it without asking me first.
    </p>
    <hr>
  '';

  renderPost = post: runCommandNoCC "${post.key}.html" {} ''
    cat ${toFile "header.html" (header post.title)} > $out

    # Write the post title & date
    echo '<article><h2 class="inline">${escape post.title}</h2>' >> $out
    echo '<aside class="date">' >> $out
    date --date="@${toString post.date}" '+%Y-%m-%d' >> $out
    echo '</aside>' >> $out

    ${
      # Add a warning to draft/unlisted posts to make it clear that
      # people should not share the post.

      if (isDraft post) then "cat ${draftWarning} >> $out"
      else if (isUnlisted post) then "cat ${unlistedWarning} >> $out"
      else "# Your ads could be here?"
    }

    # Write the actual post through cheddar's about-filter mechanism
    cat ${post.content} | ${depot.tools.cheddar}/bin/cheddar --about-filter ${post.content} >> $out
    echo '</article>' >> $out

    cat ${toFile "footer.html" footer} >> $out
  '';
in {
  inherit renderPost isDraft isUnlisted;
}
