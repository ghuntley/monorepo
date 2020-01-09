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

  # 'genLoadLisp' generates Lisp code that instructs SBCL to load all
  # the provided Lisp libraries.
  genLoadLisp = deps: lib.concatStringsSep "\n"
    (map (lib: "(load \"${lib}/${lib.lispName}.fasl\")") (allDeps deps));

  # 'genCompileLisp' generates a Lisp file that instructs SBCL to
  # compile the provided list of Lisp source files to $out.
  genCompileLisp = srcs: deps: writeText "compile.lisp" ''
    ;; This file compiles the specified sources into the Nix build
    ;; directory, creating one FASL file for each source.
    (require 'sb-posix)

    ${genLoadLisp deps}

    (defun nix-compile-lisp (file srcfile)
      (let ((outfile (make-pathname :type "fasl"
                                    :directory (or (sb-posix:getenv "NIX_BUILD_TOP")
                                                   (error "not running in a Nix build"))
                                    :defaults srcfile)))
        (multiple-value-bind (_outfile _warnings-p failure-p)
            (compile-file srcfile :output-file outfile)
          (if failure-p (sb-posix:exit 1)
              (progn
                ;; For the case of multiple files belonging to the same
                ;; library being compiled, load them in order:
                (load outfile)

                ;; Write them to the FASL list in the same order:
                (format file "cat ~a~%" (namestring outfile)))))))

    (let ((*compile-verbose* t)
          ;; FASL files are compiled into the working directory of the
          ;; build and *then* moved to the correct out location.
          (pwd (sb-posix:getcwd)))

      (with-open-file (file "cat_fasls"
                            :direction :output
                            :if-does-not-exist :create)

        ;; These forms were inserted by the Nix build:
        ${
          lib.concatStringsSep "\n" (map (src: "(nix-compile-lisp file \"${src}\")") srcs)
        }
        ))
  '';

  # 'dependsOn' determines whether Lisp library 'b' depends on 'a'.
  dependsOn = a: b: builtins.elem a b.lispDeps;

  # 'allDeps' flattens the list of dependencies (and their
  # dependencies) into one ordered list of unique deps.
  allDeps = deps: (lib.toposort dependsOn (lib.unique (
    lib.flatten (deps ++ (map (d: d.lispDeps) deps))
  ))).result;

  # 'genDumpLisp' generates a Lisp file that instructs SBCL to dump
  # the currently loaded image as an executable to $out/bin/$name.
  #
  # TODO(tazjin): Compression is currently unsupported because the
  # SBCL in nixpkgs is, by default, not compiled with zlib support.
  genDumpLisp = name: main: deps: writeText "dump.lisp" ''
    (require 'sb-posix)

    ${genLoadLisp deps}

    (let* ((bindir (concatenate 'string (sb-posix:getenv "out") "/bin"))
           (outpath (make-pathname :name "${name}"
                                   :directory bindir)))
      (save-lisp-and-die outpath
                         :executable t
                         :toplevel (function ${main})
                         :purify t))
    ;;
  '';

  # Add an `overrideLisp` attribute to a function result that works
  # similar to `overrideAttrs`, but is used specifically for the
  # arguments passed to Lisp builders.
  makeOverridable = f: orig: (f orig) // {
    overrideLisp = new: makeOverridable f (orig // (new orig));
  };

  #
  # Public API functions
  #

  # 'library' builds a list of Common Lisp files into a single FASL
  # which can then be loaded into SBCL.
  library = { name, srcs, deps ? [] }: runCommandNoCC "${name}-cllib" {} ''
    ${sbcl}/bin/sbcl --script ${genCompileLisp srcs deps}

    # FASL files can be combined by simply concatenating them
    # together, but it needs to be in the compilation order.
    mkdir $out

    chmod +x cat_fasls
    ./cat_fasls > $out/${name}.fasl
  '' // { lispName = name; lispDeps = deps; };

  # 'program' creates an executable containing a dumped image of the
  # specified sources and dependencies.
  program = { name, main ? "${name}:main", srcs, deps ? [] }: runCommandNoCC "${name}" {} ''
    mkdir -p $out/bin
    ${sbcl}/bin/sbcl --script ${
      genDumpLisp name main (lib.singleton (library {
        inherit name srcs deps;
      }))
    }
  '';

  # 'sbclWith' creates an image with the specified libraries /
  # programs loaded.
  sbclWith = deps: writeShellScriptBin "sbcl" ''
    exec ${sbcl}/bin/sbcl ${
      if deps == [] then ""
      else "--load ${writeText "load.lisp" (genLoadLisp deps)}"
    } $@
  '';
in {
  library = makeOverridable library;
  program = makeOverridable program;
  sbclWith = makeOverridable sbclWith;
}
