NixOS configuration
===================

My NixOS configuration! It configures most of the packages I require
on my systems, sets up Emacs the way I need and does a bunch of other
interesting things.

System configuration lives in folders for each machine and a custom
fixed point evaluation (similar to standard NixOS module
configuration) is used to combine configuration together.

Building `ops.nixos.depot-switcher` yields a script that will
automatically build and activate the newest configuration based on the
current hostname.

## Configured hosts:

* `nugget` - desktop computer at home
* ~~`urdhva` - T470s~~ (currently with edef)
