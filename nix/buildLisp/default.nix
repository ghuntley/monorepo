# buildLisp provides Nix functions to build Common Lisp packages,
# targeting SBCL.
#
# buildLisp is designed to enforce conventions and do away with the
# free-for-all of existing Lisp build systems.

{ pkgs ? { third_party = import <nixpkgs> {}; }
, ... }:

let
  inherit (builtins) map elemAt match;
  inherit (pkgs.third_party) lib runCommandNoCC writeText writeShellScriptBin sbcl;

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
          (multiple-value-bind (_outfile _warnings-p failure-p)
              (compile-file srcfile :output-file outfile)
            (when failure-p
              (sb-posix:exit 1)))))

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

  # 'allDeps' flattens the list of dependencies (and their
  # dependencies) into one list of unique deps.
  allDeps = deps: lib.unique (lib.flatten (deps ++ (map (d: d.lispDeps) deps)));

  # 'genLoadLisp' generates a Lisp file that instructs a Lisp to load
  # all the provided Lisp libraries.
  genLoadLisp = deps: writeText "load.lisp" (
    lib.concatStringsSep "\n" (map (lib: "(load \"${lib}/${lib.lispName}.fasl\")") (allDeps deps))
  );

  insertLibraryLoads = deps: if deps == [] then "" else "--load ${genLoadLisp deps}";

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
  library = { name, srcs, deps ? [] }: runCommandNoCC "${name}-cllib" {} ''
    ${sbcl}/bin/sbcl ${insertLibraryLoads deps} --script ${genCompileLisp srcs}

    # FASL files can be combined by simply concatenating them together:
    mkdir $out
    cat ./*.fasl > $out/${name}.fasl
  '' // { lispName = name; lispDeps = deps; };

  # 'program' creates an executable containing a dumped image of the
  # specified sources and dependencies.
  program = {};

  # 'sbclWith' creates an image with the specified libraries /
  # programs loaded.
  sbclWith = deps: writeShellScriptBin "sbcl" ''
    exec ${sbcl}/bin/sbcl ${insertLibraryLoads deps} $@
  '';
in {
  library = makeOverridable library;
  program = makeOverridable program;
  sbclWith = makeOverridable sbclWith;
}
