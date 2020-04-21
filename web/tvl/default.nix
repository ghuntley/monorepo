{ depot, pkgs, ... }:

let
  inherit (pkgs) writeText runCommandNoCC;

  homepage = writeText "index.html" ''
    <!DOCTYPE html>
    <head>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <meta name="description" content="The Virus Lounge">
      <link rel="stylesheet" type="text/css" href="/static/tvl.css" media="all">
      <link rel="icon" type="image/webp" href="/static/favicon.webp">
      <title>The Virus Lounge</title>
    </head>
    <body class="light">
      <header>
        <h1><a class="blog-title" href="/">The Virus Lounge</a> </h1>
        <hr>
      </header>

      <main>
        <img alt="The Virus Lounge" src="/static/virus_lounge.webp">
      </main>

      <p>
        Welcome to <b>The Virus Lounge</b>. We're a random group of
        people who feel undersocialised in these trying times, and
        we've decided that there isn't enough spontaneous socialising
        on the internet.
      </p>
      <p>
        Anyone can join The Virus Lounge, if it is currently open. Its
        current status is shown in the topic of the <b>##tvl</b>
        channel on Freenode.
      </p>

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
in runCommandNoCC "website" {} ''
  mkdir -p $out/static
  cp ${homepage} $out/index.html
  cp -r ${./static}/* $out/static

  # Some assets are stolen from the blog
  cp ${depot.web.homepage}/static/jetbrains-* $out/static
  cp ${depot.web.homepage}/static/tazjin.css $out/static
''
