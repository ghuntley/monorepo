{ pkgs, ... }:

pkgs.originals.ffmpeg.overrideAttrs(old: {
  buildInputs = old.buildInputs ++ [
    pkgs.cudatoolkit.out
  ];

  configureFlags = old.configureFlags ++ [
    "--enable-libnpp"
    "--enable-nonfree"
  ];
})
