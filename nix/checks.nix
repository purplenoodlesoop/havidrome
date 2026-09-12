{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs) mpv-unwrapped;
  inherit (pkgs.haskell.lib.compose)
    addTestToolDepends
    doCheck
    ;
in
{
  # `nix flake check` builds the package with its test suite enabled, on every
  # system the package exists for. The tests that drive a real player need one
  # to drive, on a null audio output: no device, but no stand-in either.
  flake.output.checks = lib.optionalAttrs (config.flake.packages ? havidrome) {
    havidrome-test = lib.pipe config.flake.packages.havidrome [
      (addTestToolDepends [ mpv-unwrapped ])
      doCheck
    ];
  };
}
