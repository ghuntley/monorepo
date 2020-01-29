buildLisp.nix
=============

This is a build system for Common Lisp, written in Nix.

It aims to offer an alternative to ASDF for users who live in a
Nix-based ecosystem. This offers several advantages over ASDF:

* Simpler (logic-less) package definitions
* Easy linking of native dependencies (from Nix)
* Composability with Nix tooling for other languages
* Effective, per-system caching strategies
* Easy overriding of dependencies and whatnot
* ... and more!

The project is still in its early stages and some important
restrictions should be highlighted:

* There is no separate abstraction for tests at the moment (i.e. they
  are built and run as programs)
* Only SBCL is supported (though the plan is to add support for at
  least ABCL and Clozure CL, and maybe make it extensible)

## Usage

`buildLisp` exposes four different functions:

* `buildLisp.library`: Builds a collection of Lisp files into a library.

  | parameter | type         | use                           | required? |
  |-----------|--------------|-------------------------------|-----------|
  | `name`    | `string`     | Name of the library           | yes       |
  | `srcs`    | `list<path>` | List of paths to source files | yes       |
  | `deps`    | `list<drv>`  | List of dependencies          | no        |
  | `native`  | `list<drv>`  | List of native dependencies   | no        |

  The output of invoking this is a directory containing a FASL file
  that is the concatenated result of all compiled sources.

* `buildLisp.program`: Builds an executable program out of Lisp files.

  | parameter | type         | use                           | required? |
  |-----------|--------------|-------------------------------|-----------|
  | `name`    | `string`     | Name of the program           | yes       |
  | `srcs`    | `list<path>` | List of paths to source files | yes       |
  | `deps`    | `list<drv>`  | List of dependencies          | no        |
  | `native`  | `list<drv>`  | List of native dependencies   | no        |
  | `main`    | `string`     | Entrypoint function           | no        |

  The `main` parameter should be the name of a function and defaults
  to `${name}:main` (i.e. the *exported* `main` function of the
  package named after the program).

  The output of invoking this is a directory containing a
  `bin/${name}`.

* `buildLisp.bundled`: Creates a virtual dependency on a built-in library.

  Certain libraries ship with Lisp implementations, for example
  UIOP/ASDF are commonly included but many implementations also ship
  internals (such as SBCLs various `sb-*` libraries).

  This function takes a single string argument that is the name of a
  built-in library and returns a "package" that simply requires this
  library.

* `buildLisp.sbclWith`: Creates an SBCL pre-loaded with various dependencies.

  This function takes a single argument which is a list of Lisp
  libraries programs or programs. It creates an SBCL that is
  pre-loaded with all of that Lisp code and can be used as the host
  for e.g. Sly or SLIME.

## Example

Using buildLisp could look like this:

```nix
{ buildLisp, lispPkgs }:

let libExample = buildLisp.library {
    name = "lib-example";
    srcs = [ ./lib.lisp ];

    deps = with lispPkgs; [
      (buildLisp.bundled "sb-posix")
      iterate
      cl-ppcre
    ];
};
in buildLisp.program {
    name = "example";
    deps = [ libExample ];
    srcs = [ ./main.lisp ];
}
```
