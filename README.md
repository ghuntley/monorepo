depot
=====

[![builds.sr.ht status](https://builds.sr.ht/~tazjin/depot/master.svg)](https://builds.sr.ht/~tazjin/depot/master?)

This repository is the [monorepo][] for my personal tools and infrastructure.
Everything in here is built using [Nix][] with an automatic attribute-set layout
that mirrors the filesystem layout of the repository (this might feel familiar
to users of Bazel).

This repository used to be hosted on GitHub, but for a variety of reasons I have
decided to take over the management of personal infrastructure - of which this
repository is a core component.

If you've ended up here and have no idea who I am, feel free to follow me [on
Twitter][].

# Highlights

## Tools

* `tools/emacs` contains my personal Emacs configuration (packages & config)
* `fun/aoc2019` contains solutions for a handful of Advent of Code 2019
  challenges, before I ran out of interest
* `tools/blog_cli` contains my tool for writing new blog posts and storing them
  in the DNS zone
* `ops/kms_pass.nix` is a tiny tool that emulates the user-interface of `pass`,
  but actually uses Google Cloud KMS for secret decryption
* `ops/kontemplate` contains my Kubernetes resource templating tool (with which
  the services in this repository are deployed!)

## Packages / Libraries

* `nix/buildGo` implements a Nix library that can build Go software in the style
  of Bazel's `rules_go`. Go programs in this repository are built using this
  library.
* `tools/emacs-pkgs` contains various Emacs libraries that my Emacs setup uses,
  for example:
  * `dottime.el` provides [dottime][] in the Emacs modeline
  * `nix-util.el` provides editing utilities for Nix files
  * `term-switcher.el` is an ivy-function for switching between vterm buffers
* `net/alcoholic_jwt` contains an easy-to-use JWT-validation library for Rust
* `net/crimp` contains a high-level HTTP client using cURL for Rust

## Services

Services in this repository are deployed on a Google Kubernetes Engine cluster
using [Nixery]().

* `web/tazblog` contains my blog software (serving at [tazj.in][])
* `web/cgit-taz` contains a slightly patched version of `cgit` that serves my
  git web interface at [git.tazj.in][]
* `ops/sync-gcsr` contains a tiny service that synchronises a Google Cloud
  Source Repository with a local disk path. My `cgit` setup uses this
  under-the-hood.
* `ops/journaldriver` contains a small Rust daemon that can forward logs from
  journald to Stackdriver Logging

## Miscellaneous

Presentations I've given in the past are in the `presentations` folder, these
cover a variety of topics and some of them have links to recordings.

There's a few fun things in the `fun/` folder, often with context given in the
README. Check out my [list of the best tools][best-tools] for example.

# Contributing

If you'd like to contribute to any of the tools in here, please check out the
[contribution guidelines](/tree/docs/CONTRIBUTING.md).

[monorepo]: https://en.wikipedia.org/wiki/Monorepo
[Nix]: https://nixos.org/nix
[on Twitter]: https://twitter.com/tazjin
[Nixery]: https://github.com/google/nixery
[tazj.in]: https://tazj.in
[git.tazj.in]: https://git.tazj.in
[best-tools]: /about/fun/best-tools/README.md
[dottime]: https://dotti.me
