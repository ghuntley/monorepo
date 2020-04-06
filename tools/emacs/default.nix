# This file builds an Emacs pre-configured with the packages I need
# and my personal Emacs configuration.
#
# On NixOS machines, this Emacs currently does not support
# Imagemagick, see https://github.com/NixOS/nixpkgs/issues/70631.
#
# Forcing Emacs to link against Imagemagick currently causes libvterm
# to segfault, which is a lot less desirable than not having telega
# render images correctly.
{ depot, ... }:

with depot;
with third_party.emacsPackages;
with third_party.emacs;

let
  emacsWithPackages = (third_party.emacsPackagesGen third_party.emacs26).emacsWithPackages;

  # $PATH for binaries that need to be available to Emacs
  emacsBinPath = lib.makeBinPath [ third_party.telega ];

  identity = x: x;

  # EXWM straight from GitHub. As of 2020-02-07, XELB in nixpkgs is
  # already at a recent enough version and does not need to be
  # overridden.
  exwmMaster = exwm.overrideAttrs(_: {
    src = third_party.fetchFromGitHub {
      owner = "ch11ng";
      repo = "exwm";
      rev = "48db94f48bea1137132345abfe8256cfc6219248";
      sha256 = "0jj12z6m5kvanq19gds3jpvid2mg8w28bbbq9iycl751y2sj4l1r";
    };
  });

  tazjinsEmacs = pkgfun: (emacsWithPackages(epkgs: pkgfun(
  # Actual ELPA packages (the enlightened!)
  (with epkgs.elpaPackages; [
    ace-window
    avy
    pinentry
    rainbow-mode
    undo-tree
  ]) ++

  # MELPA packages:
  (with epkgs.melpaPackages; [
    ace-link
    browse-kill-ring
    cargo
    clojure-mode
    cmake-mode
    counsel
    counsel-notmuch
    dash-functional
    direnv
    dockerfile-mode
    eglot
    elixir-mode
    elm-mode
    erlang
    flymake
    geiser
    go-mode
    gruber-darker-theme
    haskell-mode
    ht
    hydra
    idle-highlight-mode
    intero
    ivy
    ivy-pass
    ivy-prescient
    jq-mode
    kotlin-mode
    lispy
    lsp-mode
    magit
    markdown-toc
    multi-term
    multiple-cursors
    nginx-mode
    nix-mode
    notmuch # this comes from pkgs.third_party
    org-journal
    org-ql
    paredit
    password-store
    pg
    polymode
    prescient
    protobuf-mode
    racket-mode
    rainbow-delimiters
    refine
    request
    restclient
    sly
    string-edit
    swiper
    telega
    telephone-line
    terraform-mode
    toml-mode
    transient
    use-package
    uuidgen
    web-mode
    websocket
    which-key
    xelb
    yaml-mode
    yasnippet

    (vterm.overrideAttrs(_: {
      src = third_party.fetchFromGitHub{
        owner = "akermu";
        repo = "emacs-libvterm";
        rev = "58b4cc40ee9872a08fc5cbfee78ad0e195a3306c";
        sha256 = "1w5yfl8nq4k7xyldf0ivzv36vhz3dwdzk6q2vs3xwpx6ljy52px6";
      };
    }))
  ]) ++

  # Custom packages
  (with depot.tools.emacs-pkgs; [
    carp-mode
    exwmMaster
    dottime
    nix-util
    term-switcher

    # patched version of rcirc
    depot.third_party.emacs.rcirc
  ]))));
in lib.fix(self: l: f: third_party.writeShellScriptBin "tazjins-emacs" ''
  export PATH="${emacsBinPath}:$PATH"
  exec ${tazjinsEmacs f}/bin/emacs \
    --debug-init \
    --no-site-file \
    --no-site-lisp \
    --no-init-file \
    --directory ${./config} ${if l != null then "--directory ${l}" else ""} \
    --eval "(require 'init)" $@
  '' // {
    # Call overrideEmacs with a function (pkgs -> pkgs) to modify the
    # packages that should be included in this Emacs distribution.
    overrideEmacs = f': self l f';

    # Call withLocalConfig with the path to a *folder* containing a
    # `local.el` which provides local system configuration.
    withLocalConfig = confDir: self confDir f;

    # Build a derivation that uses the specified local Emacs (i.e.
    # built outside of Nix) instead
    withLocalEmacs = emacsBin: third_party.writeShellScriptBin "tazjins-emacs" ''
      export PATH="${emacsBinPath}:$PATH"
      export EMACSLOADPATH="${(tazjinsEmacs f).deps}/share/emacs/site-lisp:"
      exec ${emacsBin} \
        --debug-init \
        --no-site-file \
        --no-site-lisp \
        --no-init-file \
        --directory ${./config} \
        ${if l != null then "--directory ${l}" else ""} \
        --eval "(require 'init)" $@
    '';
  }) null identity
