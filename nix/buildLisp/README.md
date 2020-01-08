buildLisp.nix
=============

This is a build system for Common Lisp, written in Nix.

The project is in its early stages and currently supports nothing
other than compiling a bunch of Lisp sources into a combined FASL
file.

This is what it currently looks like:

```nix
nix.buildLisp.library {
  name = "test-lib";
  srcs = [
    ./nix/buildLisp/test-lib.lisp
  ];
}
```

Check back here in a few days for more information.
