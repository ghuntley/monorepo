# buildLisp provides Nix functions to build Common Lisp packages,
# targeting SBCL.
#
# buildLisp is designed to enforce conventions and do away with the
# free-for-all of existing Lisp build systems.

{ pkgs ? { third_party = import <nixpkgs> {}; }
, ... }:

let
  inherit (builtins) map elemAt match;
  inherit (pkgs.third_party) lib runCommand writeText sbcl;

  #
  # Internal helper definitions
  #

  # 'genCompileLisp' generates a Lisp file that instructs SBCL to
  # compile the provided list of Lisp source files to $out.
  genCompileLisp = srcs: writeText "compile.lisp" ''
      ;; This file compiles the specified sources into the Nix build
      ;; directory, creating one FASL file for each source.
      (require 'sb-posix)

      (defun nix-compile-lisp (srcfile)
        (let ((outfile (make-pathname :type "fasl"
                                      :directory (or (sb-posix:getenv "NIX_BUILD_TOP")
                                                     (error "not running in a Nix build"))
                                      :defaults srcfile)))
          (compile-file srcfile :output-file outfile)))

      (let ((*compile-verbose* t)
            ;; FASL files are compiled into the working directory of the
            ;; build and *then* moved to the correct out location.
            (pwd (sb-posix:getcwd)))

        ;; These forms were inserted by the Nix build:
        ${
          lib.concatStringsSep "\n" (map (src: "(nix-compile-lisp \"${src}\")") srcs)
        }
      )
    '';

  #
  # Public API functions
  #

  # Add an `overrideLisp` attribute to a function result that works
  # similar to `overrideAttrs`, but is used specifically for the
  # arguments passed to Lisp builders.
  makeOverridable = f: orig: (f orig) // {
    overrideLisp = new: makeOverridable f (orig // (new orig));
  };

  # 'library' builds a list of Common Lisp files into a single FASL
  # which can then be loaded into SBCL.
  library = { name, srcs, deps ? [] }: runCommand "${name}-cllib" {} ''
    ${sbcl}/bin/sbcl --script ${genCompileLisp srcs}

    # FASL files can be combined by simply concatenating them together:
    mkdir $out
    cat ./*.fasl > $out/${name}.fasl
  '';

  # 'program' creates an executable containing a dumped image of the
  # specified sources and dependencies.
  program = {};

  # 'sbclWith' creates an image with the specified libraries /
  # programs loaded.
  sbclWith = {};
in {
  library = makeOverridable library;
  program = makeOverridable program;
  sbclWith = makeOverridable sbclWith;
}
