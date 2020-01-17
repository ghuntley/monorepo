# This file builds an Emacs pre-configured with the packages I need
# and my personal Emacs configuration.
#
# On NixOS machines, this Emacs currently does not support
# Imagemagick, see https://github.com/NixOS/nixpkgs/issues/70631.
#
# Forcing Emacs to link against Imagemagick currently causes libvterm
# to segfault, which is a lot less desirable than not having telega
# render images correctly.
{ pkgs, ... }:

with pkgs;
with third_party.emacsPackagesNg;
with third_party.emacs;

let
  emacsWithPackages = (third_party.emacsPackagesNgGen third_party.emacs26).emacsWithPackages;

  # $PATH for binaries that need to be available to Emacs
  emacsBinPath = lib.makeBinPath [ third_party.telega ];

  identity = x: x;
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
    browse-kill-ring
    cargo
    clojure-mode
    counsel
    counsel-notmuch
    dash-functional
    direnv
    dockerfile-mode
    elixir-mode
    elm-mode
    erlang
    exwm
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
    paredit
    password-store
    pg
    prescient
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
  (with pkgs.tools.emacs-pkgs; [
    carp-mode
    dottime
    nix-util
    term-switcher
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
